package producerconsumer;
//the Worker does some work (if there is any)

import java.util.concurrent.BlockingQueue;

class Worker implements Runnable{

	private BlockingQueue<Integer> queue;
	private int id;

	Worker(BlockingQueue<Integer> q, int i){
		queue = q;
		id = i;
		new Thread(this).start();
	}

	public void run() {
		while(true){
			try {
				System.out.println("Worker "+id+" did task: "+queue.take());
			}catch(InterruptedException ie){}
		}
	}
}
