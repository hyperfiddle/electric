package hyperfiddle.electric;
import clojure.lang.IPersistentVector;
import clojure.lang.IExceptionInfo;
import clojure.lang.IPersistentMap;

/*
  Like ExceptionInfo, but for electric failure.
  Does not allocate a stacktrace.
 */
public class FailureInfo extends RuntimeException implements IExceptionInfo{
    public final IPersistentMap data;

    public FailureInfo(String s, IPersistentMap data) {
        this(s, data, null);
    }

    public FailureInfo(String s, IPersistentMap data, Throwable throwable) {
        super(s, throwable, false, false);
        if (data != null) {
            this.data = data;
        }  else {
            throw new IllegalArgumentException("Additional data must be non-nil.");
        }
    }

    public IPersistentMap getData() {
        return data;
    }

    public String toString() {
        return "hyperfiddle.electric.FailureInfo: " + getMessage() + " " + data.toString();
    }

    public boolean equals(Object o){
        return (o instanceof FailureInfo) && (this.getCause().equals(((FailureInfo) o).getCause())) ;
    }

}
