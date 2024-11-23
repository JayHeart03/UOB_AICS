package producerconsumer;
//the Manager makes work (Producer)
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

class Manager implements Runnable{
	
	private BlockingQueue<Integer> queue;
	private int id;

	Manager(BlockingQueue<Integer> q, int i){
		queue = q;
		id = i;
		new Thread(this).start();
	}

	public void run() {
		//how much work
		int j=10000;
		for(int i=0; i<=j; i++){
			try{
				queue.put(i);
				System.out.println("Boss "+id+" set task: "+i);
			}catch(InterruptedException ie){}
		}
	}

	public static void main(String args[]) {
		//work is done via a work box
		BlockingQueue<Integer> workbox = new LinkedBlockingQueue<Integer>(1);

		new Manager(workbox,1);
		new Worker(workbox, 1);
		new Worker(workbox, 2);
	}
}

