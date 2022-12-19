package bites.idz;

import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.math.BigInteger;
import java.time.Instant;
import java.util.Arrays;

public class UUIDV7 implements Externalizable, Comparable<UUIDV7> {

    private final byte[] raw;
    public static final int SIZE = 16;

    public UUIDV7(byte[] raw) {
        if (raw.length != SIZE)
            throw new IllegalArgumentException("Invalid number of raw bytes: " + raw.length);
        this.raw = raw;
    }
    public UUIDV7() {
        this.raw = new byte[SIZE];
    }

    public byte[] toByteArray() {
        return raw.clone();
    }

    public static UUIDV7 fromString (String s){
        if (s.length() != 36)
            throw new IllegalArgumentException("Invalid UUID String representation!");

        String noDashes = s.replace("-","");
        byte[] raw = new byte[SIZE];
        for (int i=0;i < SIZE; i++){
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
        var thisTS  = new BigInteger(1, Arrays.copyOfRange(raw, 0 ,6));
        var otherTS = new BigInteger(1, Arrays.copyOfRange(otherBS, 0, 6));
        var tsRet = thisTS.compareTo(otherTS);
        ret = tsRet;

        if (tsRet == 0) {
            var thisSeq  = new BigInteger(1, Arrays.copyOfRange(raw, 6 ,8));
            var otherSeq = new BigInteger(1, Arrays.copyOfRange(otherBS, 6, 8));
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
