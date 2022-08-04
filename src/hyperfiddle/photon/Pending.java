package hyperfiddle.photon;
public class Pending extends Throwable {
    public boolean equals(Object o){
        return (o instanceof Pending);
    }
}
