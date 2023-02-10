package hyperfiddle.electric;
public class Pending extends Throwable {
    public Pending() {
        super(null, null, false, false);
    }

    public boolean equals(Object o){
        return (o instanceof Pending);
    }
}
