package diningphiliosophers;
public class Philosopher extends Thread {
  private int id;
  private Object firstFork, secondFork;

  public Philosopher(int id, Object firstFork, Object secondFork) {
    this.id = id;
    this.firstFork = firstFork;
    this.secondFork = secondFork;
  }

  public void run () {
    for (int i = 0; true; i++) {
      synchronized (firstFork) {
        System.out.println("Philosopher " + id + " grabbed a fork");

        synchronized (secondFork) {
          System.out.println(id + " grabbed the other fork");
          System.out.println(id + " is eating for the " + i + " time");

          try { 
              sleep(4); // Eating takes some time.
          }
          catch (InterruptedException e) {
          }    
        }

        System.out.println("Philosopher " + id + " released a fork");
      }

      System.out.println("Philosopher " + id + " released the other fork");
    }
  }
}
