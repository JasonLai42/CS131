class UnsynchronizedState implements State {
    private byte[] value;
    private byte maxval;

    UnsynchronizedState(byte[] v) { value = v; maxval = 127; }

    UnsynchronizedState(byte[] v, byte m) { value = v; maxval = m; }

    public int size() { return value.length; }

    public byte[] current() { return value; }

    /* No synchronized keyword */
    public boolean swap(int i, int j) {
        if (value[i] <= 0 || value[j] >= maxval) {
            return false;
        }
        value[i]--;
        value[j]++;
        return true;
    }
}
