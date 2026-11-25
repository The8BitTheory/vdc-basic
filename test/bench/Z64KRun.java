// Startet Z64K im selben Prozess und sichert beim Beenden das Diskimage von Laufwerk 8.
// Z64K schreibt Diskimages nie selbst zurueck (nur ueber den GUI-Dialog); die Beendigung per
// -debugcart ($D7FF) oder -limitcycles laeuft ueber System.exit, deshalb genuegt ein Shutdown-Hook.
//
//   Z64K_SAVE=out.d64 java -cp Z64K.jar test/bench/Z64KRun.java c128 -vdc64kb ... -8 bench.d64 ...
//
// Zugriff per Reflection auf interne Klassen (getestet mit der Z64K.jar vom 2026-11):
// portal.Z64K.main, config.ConfigManager.getInstance() -> Feld "config" (Map) -> "config_<maschine>"
// (CBMConfig) .saveDisk(Pfad, 8). Fehlt eines davon (andere Z64K-Version), meldet Z64KRun das mit
// "Z64KRun:" und Exitcode 3 (Start) bzw. ohne gesichertes Image (run_bench.py bricht dann ab).
import java.lang.reflect.*;
import java.util.*;

public class Z64KRun {
  public static void main(String[] a) throws Throwable {
    final String out = System.getenv("Z64K_SAVE");
    final String key = "config_" + (a.length > 0 ? a[0] : "c128");
    Method m;
    try {
      m = Class.forName("portal.Z64K").getDeclaredMethod("main", String[].class);
      m.setAccessible(true);
    } catch (ReflectiveOperationException | RuntimeException e) {
      System.out.println("Z64KRun: Einstieg portal.Z64K.main nicht gefunden (andere Z64K-Version?): " + e);
      System.exit(3);
      return;
    }
    if (out != null) {
      Runtime.getRuntime().addShutdownHook(new Thread(() -> {
        try {
          Object cm = Class.forName("config.ConfigManager").getMethod("getInstance").invoke(null);
          Field f = cm.getClass().getDeclaredField("config");
          f.setAccessible(true);
          Object cfg = ((Map<?, ?>) f.get(cm)).get(key);
          if (cfg == null)
            throw new IllegalStateException("kein Eintrag " + key + " in ConfigManager.config");
          cfg.getClass().getMethod("saveDisk", String.class, int.class).invoke(cfg, out, 8);
          System.out.println("Z64KRun: Image gesichert: " + out);
        } catch (Throwable t) {
          System.out.println("Z64KRun: Sichern fehlgeschlagen (interne Z64K-API geaendert?): " + t);
          t.printStackTrace(System.out);
        }
      }));
    }
    try {
      m.invoke(null, (Object) a);
    } catch (InvocationTargetException e) {
      throw e.getCause();
    }
  }
}
