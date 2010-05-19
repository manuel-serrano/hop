package bigloo.hop;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import bigloo.hop.*;

/**
   This class is instantiated when Apache Felix creates a bundle.
   We create a HopThread and start it.
   */
public class HopActivator implements BundleActivator {
  private BundleContext context;

  public void start(BundleContext context) throws Exception {
    System.out.println("Starting: Hop Activator");
    this.context = context;

    String []args= {};
    HopThread ht= new HopThread (args);
    ht.start ();
  }

  public void stop(BundleContext context) throws Exception {
    System.out.println("Stopping: Goodbye Cruel World");
    // TODO: stop hop
    this.context = null;
  }
}
