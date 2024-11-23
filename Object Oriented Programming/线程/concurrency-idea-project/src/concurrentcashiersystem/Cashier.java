package concurrentcashiersystem;

public class Cashier implements Runnable {

    private String name;
    private Account account;

    public Cashier(String name, Account account) {
        this.name = name;
        this.account = account;
    }

    private void tell(String message) {
        System.out.println(message);
    }

    public void run() {
        tell("Worker " + name + " got started");

        for (int i = 0; i < 1000000; i++) {
            account.increment();
            account.decrement();
        }

        tell("Worker " + name + " finished");
    }
}
