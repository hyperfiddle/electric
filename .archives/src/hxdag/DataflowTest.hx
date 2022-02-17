package hyperfiddle;
import hyperfiddle.Dataflow;

class DataflowTest {
  static function main() {
    var a = Origin.input();
    var b = Origin.input();

    var am = Origin.apply([a], x -> '$x!');
    var ab = Origin.apply([am, b], (a, b) -> [a,b]);

    Origin.on(am, x -> trace("map:", x));
    Origin.on(ab, x -> trace("join:", x));

    var x1 = Origin.apply([a, am], (a, b) -> [a,b]);
    var x2 = Origin.apply([a, ab, b], (a, b, c) -> [a,b,c]);

    Origin.on(x1, x -> trace("join2:", x));
    Origin.on(x2, x -> trace("join3:", x));

    a.put("a");
    b.put("b");
    a.put("a2");
    b.put("b2");
    b.end();
    a.put("a3");

    Origin.all(() -> {
      a.put("az");
      b.put("bz");
    });
  }
}
