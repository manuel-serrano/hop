package bigloo.hop;

import bigloo.*;

/**
   This class knows all the tricks needed to boot hop from a Java main function.
   */
public class HopRunner extends Object {
  public static int run (String []args) {
    // init all the modules before calling main
    bigloo.hop.Main.bigloo_dlopen_init ();
    // and fake a proper initialization
    // bigloo.runtime.Llib.param.bigloo-initialized! ();
    // yes, 'tis mangled
    bigloo.runtime.Llib.param.BgL_bigloozd2initializa7edz12z67 ();

    Object list= foreign.listargv (args);
    return bigloo.hop.Main.boot (list);
  }
}
