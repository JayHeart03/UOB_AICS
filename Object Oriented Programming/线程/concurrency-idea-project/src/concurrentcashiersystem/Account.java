package concurrentcashiersystem;

public class Account {

    private int balance = 0;

    public void increment() {
        balance++;
    }

    public void decrement() {
        balance--;
    }

    public int get() {
        return balance;
    }
}


