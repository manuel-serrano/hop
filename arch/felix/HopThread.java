package bigloo.hop;

import bigloo.hop.*;

/**
   This subclass of Thread just delegates all the burden to HopRunner.
   We splited the code this much for testability of individual parts.
   */
public class HopThread extends Thread {
  String []args;

  HopThread (String []args) {
    this.args= args;
  }

  public void run () {
    bigloo.hop.HopRunner.run (this.args);
  }
}
