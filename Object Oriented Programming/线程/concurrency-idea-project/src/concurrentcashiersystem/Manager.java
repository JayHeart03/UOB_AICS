package concurrentcashiersystem;

public class Manager {

    private static void tell(String message) {
        System.out.println(message);
    }

    public static void main(String args[]) {
        tell("Manager got started");

        Account budget = new Account();

        Thread[] staff = new Thread[args.length];

        for (int i = 0; i < args.length; i++) {
            tell("Manager is starting Cashier " + args[i]);
            staff[i] = new Thread(new Cashier(args[i], budget));
            staff[i].start();
        }

        tell("Manager will now wait for each Cashier to finish");

        try {
            for (Thread sm : staff)
                sm.join();
        } catch (InterruptedException e) {
            tell("Interruption before completion of the joins" + e);
            tell("Manager has given up waiting for Cashier");
        }

        tell("balance should be 0 and actually is " + budget.get());

        tell("Manager is exiting");
    }
}
