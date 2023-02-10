package hyperfiddle.electric;
public class Failure {
    public final Throwable error;
    public Failure(Throwable e) {
        this.error = e;
    }
    public boolean equals(Object o){
        return (o instanceof Failure) && (this.error.equals(((Failure) o).error));
    }
}
