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

    private static String padBits (String bits, int targetLength){
        String pad = "0".repeat(targetLength - bits.length());
        return pad + bits;
    }

    private static byte[] bitsToBytes (String bits){
        int bitsLength = bits.length();
        if (bitsLength % 8 != 0)
            throw new IllegalArgumentException("<bits> length is not cleanly divisible by 8: " + bitsLength);
        int bytesNumber = bitsLength / 8;
        byte[] ret = new byte[bytesNumber];
        for (int i=0; i<bytesNumber; i++){
            int startIdx = i*8;
            String byteBits = bits.substring(startIdx, startIdx+8);
            int uint = Integer.parseInt(byteBits, 2);
            ret[i] = (uint > Byte.MAX_VALUE) ? (byte)(uint - 256) : (byte) uint;
        }
        return ret;
    }

    private static String randomBits (int n){
        BigInteger randomNum = new BigInteger(n, randomSource);
        String bits = randomNum.toString(2);
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
    private static byte[] genBytes (long epochMilli, String counterBits){
        String tsBits = Long.toBinaryString(epochMilli);
        int tsBitsLength = tsBits.length(); // this can't be more than 64

        // https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#section-6.1-2.14
        if (tsBitsLength > UNIX_TS_MS_BIT_COUNT)
            // keep the 48 least significant bits
            tsBits = tsBits.substring(tsBitsLength - UNIX_TS_MS_BIT_COUNT);

        // https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#section-6.1-2.12
        if (tsBitsLength < UNIX_TS_MS_BIT_COUNT)
            // pad the most significant bits
           tsBits = padBits(tsBits, UNIX_TS_MS_BIT_COUNT);

        String randA = counterBits == null ? randomBits(12) : padBits(counterBits, 12);
        String randB = randomBits(62);

        return bitsToBytes(tsBits + VERSION_BITS + randA + VARIANT_BITS + randB);

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
     * @return a Supplier of UUIDV7 objects with monotonically increasing counter bits.
     * The values supplied are guaranteed to be sortable, even in the face of timestamp collisions.
     */
    public static Supplier<UUIDV7> supplier (){
        AtomicLong prevTS = new AtomicLong(-1);
        AtomicInteger counter = new AtomicInteger(0); // a bit of an overkill for 12 bits still :(

        return () -> {
            Instant now = Instant.now();
            long nowMilli = now.toEpochMilli();
            long previousMilli = prevTS.get();
            int seq;
            if (nowMilli == previousMilli)
                seq = counter.getAndIncrement();
            else if (nowMilli > previousMilli) {
                prevTS.set(nowMilli);
                counter.set(seq = 0);
            }
            else {
                // seems like the clock has moved back
                // https://www.ietf.org/archive/id/draft-peabody-dispatch-new-uuid-format-04.html#section-6.1-2.10
                long diff = Duration.between(Instant.ofEpochMilli(previousMilli) , now).toNanos();
                if (diff > 10 * 1000_000_000L){ // NANOS_PER_SECOND
                    // clock moved back by more than 10 seconds - this is bad!
                    // something must have happened to the machine - reset everything
                    prevTS.set(nowMilli);
                    counter.set(seq = 0);
                }
                // clock moved back by less than 10 seconds
                // not sure how this can happen and/or what to do
                else
                    throw new IllegalStateException("Clock seems to have moved back by less than 10 seconds (not long enough to consider it machine-wide)!");

            }
            // we're finally ready to generate the actual bytes
            if (seq < 4096) // we have 12 bits available (i.e. 2^12)
             return new UUIDV7(genBytes(nowMilli, Integer.toBinaryString(seq)));
            else // counter overflow (highly unlikely) - pretend we're on the next clock tick
             return new UUIDV7(genBytes(nowMilli + 1, "0"));
        };
    }

    public static UUIDV7 fromString (String s){
        assert (s.length() == 36);

        String noDashes = s.replace("-","");
        byte[] raw = new byte[SIZE];
        for (int i=0; i < SIZE; i++){
            String octet = noDashes.substring(2*i , 2*(i + 1));
            int parsed = Integer.parseInt(octet, 16);
            int ret = parsed >  127 ? parsed - 256 : parsed;
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
    public int compareTo(UUIDV7 o) {
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
        int sidx = 0;
        StringBuilder sb = new StringBuilder(36);
        for (int i=0; i<SIZE; i++){
            String hexDigits = Integer.toHexString(Byte.toUnsignedInt(raw[i]));
            sb.append(hexDigits.length() == 1 ? "0" + hexDigits : hexDigits);
            sidx+=2;
            if (sidx == 8 || sidx == 13 || sidx == 18 || sidx == 23){
                sb.append('-');
                sidx++;
            }
        }
        return sb.toString();
    }
}
