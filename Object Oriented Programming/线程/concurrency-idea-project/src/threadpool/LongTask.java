package threadpool;

class LongTask implements Runnable{

	private int id;

	LongTask(int i){
		id=i;
	}

	public void run() {
		System.out.println("Long task "+id+" started");

		int maxCount = 1000;
		// do some long task
		for (int i = 0; i < maxCount; i++) {
			System.out.println(i);
			try {
				Thread.sleep(1);
			}catch(Exception e){
			}
		}

		System.out.println("Long task "+id+"  done.");
	}
}