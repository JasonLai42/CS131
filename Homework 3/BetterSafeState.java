import java.util.concurrent.locks.ReentrantLock;

/* Use ReentrantLocks; better than StampedLocks to avoid deadlock */
class BetterSafeState implements State {
    private byte[] value;
    private byte maxval;
    private final ReentrantLock lock;

    BetterSafeState(byte[] v) { 
	value = v; 
	maxval = 127; 
	lock = new ReentrantLock(); 
    }

    BetterSafeState(byte[] v, byte m) { 
	value = v; 
	maxval = m; 
	lock = new ReentrantLock(); 
    }

    public int size() { return value.length; }

    public byte[] current() { return value; }

    public boolean swap(int i, int j) {
	lock.lock();
	if (value[i] <= 0 || value[j] >= maxval) {
	    lock.unlock();
	    return false;
	}
	value[i]--;
	value[j]++;
	lock.unlock();
	return true;
    }
}
