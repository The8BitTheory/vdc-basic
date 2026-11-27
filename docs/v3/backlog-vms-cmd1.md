# Deferred: making VMS command 1 faster

As of: 2026-11-26. **Status: deferred.** There is currently no caller for which it is worthwhile: Crawl 1.1 uses no VMS command 1 in the walkthrough (only VMC type 2 and VMF type 5). Resume when a game with many transparent wall edges, grilles or sprites (Engine 0.2, `byte_edges` off) runs into the VMS time budget.

## Findings

VMS command 1 copies a run of a few bytes (the opaque pieces of a sprite row). Measured (P6g, `dungeonboringengine/docs/specs/P6g-vcl-analysis.md`, before P2f): **≈ 308 cycles per command** (bench v2h: 318). The bytes themselves cost almost nothing; the fixed costs per command are what is expensive. Since P2f, command 1 also runs through the new row loop (`block_copy_target`); a value after P2f is **not measured** – measure once before optimizing (only the new version, before-value = this state).

Where the cycles go (`src/vdcbasic.asm`, `.vms_arg_loop` … `.vms_check_more`):

1. **Reading the list and dispatching (≈ 100 cycles):**
   - Before every command the list pointer is recomputed with 16 bits (add `offset_1` to `arg_address`, `offset_1 = 0`) instead of continuing to read with Y.
   - Dispatcher as a comparison chain (`cmp #1 / #2 / #3 / #8`).
   - Target offset added to the target address with 16 bits.
   - Source address tracked in software (`vms_block_source`), although the VDC advances it itself for command 1; it is only needed for command 3 (source skip) and 8 (half byte).
2. **General VMC path for exactly one row:** command 1 sets length and row count to 1 (`arg3`, `arg4`) and jumps into `block_copy_target` – with the decisions for multiple rows (source step, self-modified jumps, page path from 256 bytes), although it is always a short row.
3. **Two VDC waits per command:** for the read-ahead after R19 (before R30) and for the end of the previous copy. With runs of 1–6 bytes the CPU waits for this on every command; in chunky mode the VDC has only ≈ 16 free memory cycles per character row during the display (P2g), so the copy then takes up to ≈ 127 µs.
4. **Loop end:** count down the 16-bit command counter, jump back.

## Possible measures

- **Dedicated tight loop for command 1:** keep the list pointer in Y (only carry over on overflow), check command 1 as the first case, write R18/R19/R30 directly (without `block_copy_target`), source tracking only for commands 3 and 8. Waits only where P2g shows them to be necessary (before the first access after R30 and before R30).
- **More compact list format (bigger change):** target offset as an 8-bit distance to the previous run instead of a 16-bit absolute value. Saves reading and addition, but is a format change (`docs/reference/vms-format.md`); the engine (`clipvms_e`, reference `dbeview`) and converters (`dbetex`, `dbespr`) would have to follow.
- **Estimate:** about half of today's cost per command. Unproven, only after measuring.

## Related

- `docs/v3/P2f-vmc-loop.md` (row loop VMC/VMF), `docs/v3/P2g-wait-experiment.md` (which waits are necessary, fast path `R-ACt`, reg 36).
- VMS command 8 (half byte, ≈ 706 cycles, P6g) is the second expensive VMS path (wall edges with pixel-exact edges); same fixed costs plus two VRAM read accesses and one write access.
