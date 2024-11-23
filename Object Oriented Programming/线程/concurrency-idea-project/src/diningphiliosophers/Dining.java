package diningphiliosophers;
public class Dining {

    public static void main(String [] args) {
			Object forkA = new Object();
			Object forkB = new Object();
			Object forkC = new Object();
			Object forkD = new Object();
			Object forkE = new Object();

			Philosopher phil0 = new Philosopher(0,forkA,forkB);
			Philosopher phil1 = new Philosopher(1,forkB,forkC);
			Philosopher phil2 = new Philosopher(2,forkC,forkD);
			Philosopher phil3 = new Philosopher(3,forkD,forkE);
			Philosopher phil4 = new Philosopher(4,forkE,forkA);

			phil0.start();
			phil1.start();
			phil2.start();
			phil3.start();
			phil4.start();
    }
}
