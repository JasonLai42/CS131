import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    GetNSetState(byte[] v) { 
	value = new AtomicIntegerArray(byte_to_int(v)); 
	maxval = 127; 
    }

    GetNSetState(byte[] v, byte m) { 
	value = new AtomicIntegerArray(byte_to_int(v)); 
	maxval = m; 
    }

    public int size() { return value.length(); }

    public byte[] current() { return int_to_byte(value); }

    public boolean swap(int i, int j) {
        if (value.get(i) <= 0 || value.get(j) >= maxval) {
            return false;
        }
        value.set(i, value.get(i)-1);
        value.set(j, value.get(j)+1);
        return true;
    }

    /* Auxiliary Functions */

    /* Convert the byte array to an int array */
    private int[] byte_to_int(byte[] v) {
        int[] int_v = new int[v.length];
        for(int i = 0; i < v.length; i++) {
            int_v[i] = (int) v[i];
        }
        return int_v;
    }

    /* Convert the AtomicIntegerArray to a byte array */
    private byte[] int_to_byte(AtomicIntegerArray v) {
	byte[] byte_v = new byte[v.length()];
	for(int i = 0; i < v.length(); i++) {
	    byte_v[i] = (byte) v.get(i);
	}
	return byte_v;	
    }
}
