package threadpool;

import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;

public class Main{

  public static void main(String args[]) {
    ExecutorService tpe = Executors.newFixedThreadPool(3);

    tpe.submit(new LongTask(1));
    tpe.submit(new LongTask(2));
    tpe.submit(new LongTask(3));

    tpe.shutdown();
  }
}