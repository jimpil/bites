package bites.idz;

import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.math.BigInteger;
import java.security.SecureRandom;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Supplier;

/**
 * An immutable UUID version 7
 */
public class UUIDV7 implements Externalizable, Comparable<UUIDV7> {

    private final byte[] raw;
    private static final int UNIX_TS_MS_BIT_COUNT = 48;
    private static final String VERSION_BITS = "0111";
    private static final String VARIANT_BITS = "10";
    private static final int SIZE = 16;
    private static final SecureRandom randomSource = new SecureRandom();

    private UUIDV7(byte[] raw) { this.raw = raw; }
    public UUIDV7() { // need this for Externalizable
      this(new byte[SIZE]);
    }

    public byte[] toByteArray() {
        return raw.clone();
    }

    private static String padBits (final String bits, final int targetLength){
        final String pad = "0".repeat(targetLength - bits.length());
        return pad + bits;
    }

    private static byte[] bitsToBytes (final String bits){
        final int bitsLength = bits.length();
        if (bitsLength % 8 != 0)
            throw new IllegalArgumentException("<bits> length is not cleanly divisible by 8: " + bitsLength);

        final int bytesNumber = bitsLength / 8;
        final byte[] ret = new byte[bytesNumber];
        for (int i=0; i<bytesNumber; i++){
            int startIdx = i*8;
            String byteBits = bits.substring(startIdx, startIdx+8);
            int uint = Integer.parseInt(byteBits, 2);
            ret[i] = (uint > Byte.MAX_VALUE) ? (byte)(uint - 256) : (byte) uint;
        }
        return ret;
    }

    private static String randomBits (final int n){
        final BigInteger randomNum = new BigInteger(n, randomSource);
        final String bits = randomNum.toString(2);
        return (n > bits.length() ? padBits(bits, n) : bits);
    }

    /**
     * Per <a href="https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#name-uuid-version-7">spec</a>
     * 0                   1                   2                   3
     * 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     * |                           unix_ts_ms                          |
     * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     * |          unix_ts_ms           |  ver  |       rand_a          |
     * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     * |var|                        rand_b                             |
     * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     * |                            rand_b                             |
     * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     * @param epochMilli - The standard UNIX-epoch timestamp
     * @param counterBits - The bits of the counter (if there is one), or null
     * @return The 16 bytes that will make up this UUID
     */
    private static byte[] genBytes (final long epochMilli, final String counterBits){
        String tsBits = Long.toBinaryString(epochMilli);
        final int tsBitsLength = tsBits.length(); // this can't be more than 64

        // https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#section-6.1-2.14
        if (tsBitsLength > UNIX_TS_MS_BIT_COUNT)
            // truncate the least significant bits
            tsBits = tsBits.substring(0, UNIX_TS_MS_BIT_COUNT);

        // https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#section-6.1-2.12
        if (tsBitsLength < UNIX_TS_MS_BIT_COUNT)
            // pad the most significant bits
           tsBits = padBits(tsBits, UNIX_TS_MS_BIT_COUNT);

        final String randA = counterBits == null ? randomBits(12) : padBits(counterBits, 12);
        final String randB = randomBits(62);

        return bitsToBytes(tsBits + VERSION_BITS + randA + VARIANT_BITS + randB);

    }

    /**
     *
     * @return a random int from 0 (inclusive) to 2048 (exclusive)
     */
    private static int randomCounter(){
        return new BigInteger(11, randomSource).intValue();
    }

    /**
     *
     * @return a random int from 0 (inclusive) to 32 (exclusive)
     */
    public static int randomIncrement(){
        return new BigInteger(5, randomSource).intValue();
    }

    /**
     *
     * @return a new UUIDV7 object with random counter bits.
     * Avoid using this for generating ids in quick succession, or in parallel.
     */
    public static UUIDV7 supply1 (){
        return new UUIDV7(genBytes(Instant.now().toEpochMilli(), null));
    }

    /**
     *
     * @param clockDriftCutOff
     * The minimum number of nanoseconds the clock is allowed to drift (into the past)
     * before it is considered an illegal state. For example, a value of 10 seconds means
     * that any drift (into the past) greater than 10 seconds is considered 'machine-wide',
     * and so the internal state is reset, whereas for a drift less than 10 seconds,
     * an IllegalStateException is thrown. A value of 0 essentially means 'never-throw'
     * (any drift into the past is valid).
     *
     * @return a Supplier of UUIDV7 objects with monotonically increasing counter bits.
     * The values supplied are guaranteed to be sortable, even in the face of timestamp collisions.
     */
    public static Supplier<UUIDV7> supplier (final long clockDriftCutOff){
        final AtomicLong prevTS = new AtomicLong(-1);
        final AtomicInteger counter = new AtomicInteger(0); // Monotonic Random (Method 2)

        return () -> {
            final Instant now = Instant.now();
            final long nowMilli = now.toEpochMilli();
            final long previousMilli = prevTS.get();

            if (nowMilli == previousMilli)
                //  The increment value for every UUID generation SHOULD be
                //  a random integer  of any desired length larger than zero
                counter.addAndGet(Math.max(1, randomIncrement())); // 1 - 15
            else if (nowMilli > previousMilli) {
                // the very first call will land here
                prevTS.set(nowMilli);
                counter.set(randomCounter());
            }
            else {
                // seems like the clock has moved back
                // https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#section-6.1-2.10
                long diff = Duration.between(Instant.ofEpochMilli(previousMilli), now).toNanos();
                if (diff > clockDriftCutOff){
                    // clock moved back by more than the cutoff (e.g. 10 seconds) - this is bad!
                    // something must have happened to the machine - reset everything
                    prevTS.set(nowMilli);
                    counter.set(randomCounter());
                }
                else // clock moved back by less than the cutoff - throw
                    throw new IllegalStateException("Clock seems to have moved back by less than " + clockDriftCutOff + " nanos.");

            }
            final int seq = counter.get();
            //System.out.println(seq);
            // we're finally ready to generate the actual bytes
            if (seq < 4096) // we have 12 bits available (i.e. 2^12)
                return new UUIDV7(genBytes(nowMilli, Integer.toBinaryString(seq)));
            else {
                // counter overflow - extremely unlikely because the counter starts at less than 2048,
                // and increments by less than 32 each time (it would take hundreds of increments to reach 4096)
                // if it does happen pretend we're on the next available tick and use the minimum possible counter
                counter.set(0);
                return new UUIDV7(genBytes(nowMilli + 1, "0"));
            }
        };
    }

    public static Supplier<UUIDV7> supplier (){
        return supplier(10 * 1000_000_000L); // // NANOS_PER_SECOND
    }

    private static void checkVersion(final int x){
        if (!Integer.toBinaryString(x).startsWith("111"))
            throw new IllegalStateException("Version mismatch - this is NOT a version 7 UUID!");
    }

    public static UUIDV7 fromString (final String s){
        if (s.length() != 36)
            throw new IllegalArgumentException("UUID textual representation should be 36 characters long.");

        String noDashes = s.replace("-","");
        final byte[] raw = new byte[SIZE];
        for (int i=0; i < SIZE; i++){
            // get the right 2-char slice (1 byte = 2 chars in hex)
            String octet = noDashes.substring(2*i , 2*(i + 1));
            // parse to in an unsigned byte
            int parsed = Integer.parseInt(octet, 16);
            // check the 7th byte for correct version (i.e. 0111)
            if (i == 6) checkVersion(parsed);
            // convert it to a signed byte
            int ret = parsed > Byte.MAX_VALUE ? parsed - 256 : parsed;
            raw[i] = (byte)ret;
        }
        return new UUIDV7(raw);
    }

    public Instant createdAt() {
        return Instant.ofEpochMilli(
                new BigInteger(1, Arrays.copyOfRange(raw, 0 ,6))
                        .longValue()
        );
    }

    @Override
    public void writeExternal(ObjectOutput out) throws IOException {
        out.write(raw);
        out.flush();
    }

    @Override
    public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
        int read = in.read(raw);
        if (read != SIZE)
            throw new IllegalStateException("Not enough bytes read: " + read);

    }

    @Override
    public int compareTo(final UUIDV7 o) {
        int ret;
        byte[] otherBS = o.toByteArray();
        BigInteger thisTS  = new BigInteger(1, Arrays.copyOfRange(raw, 0 ,6));
        BigInteger otherTS = new BigInteger(1, Arrays.copyOfRange(otherBS, 0, 6));
        int tsRet = thisTS.compareTo(otherTS);
        ret = tsRet;

        if (tsRet == 0) { // we've hit a collision - use counter
            BigInteger thisSeq  = new BigInteger(1, Arrays.copyOfRange(raw, 6 ,8));
            BigInteger otherSeq = new BigInteger(1, Arrays.copyOfRange(otherBS, 6, 8));
            ret = thisSeq.compareTo(otherSeq);
        }
        return ret;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(raw);
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof UUIDV7)
            return Arrays.equals(raw, ((UUIDV7) o).toByteArray());
        else
            return false;
    }
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder(36);
        for (int i=0; i<SIZE; i++){
            String hexDigits = Integer.toHexString(Byte.toUnsignedInt(raw[i]));
            sb.append(hexDigits.length() == 1 ? "0" + hexDigits : hexDigits);
            final int length = sb.length();
            if (length == 8 || length == 13 || length == 18 || length == 23)
                sb.append('-');

        }
        return sb.toString();
    }
}
