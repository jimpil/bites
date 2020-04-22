# bites

## What

A no-brainer Clojure library for easily turning (certain) things into bytes (and back), 
which you can wrap your head around in less than 15 minutes. 
Emphasis is on correctness rather than speed (i.e. byte arrays/buffers are not reused). 
Generative testing through [test.check](https://github.com/clojure/test.check), 
and where appropriate against [commons-codec](http://commons.apache.org/proper/commons-codec/).


## Why
Every once in a while I'm finding myself looking online about how to turn 
certain objects into bytes, and I've done it so many times now that I am 
starting to memorize the various articles/posts I encounter over and over again.
Yes, [commons-codec](http://commons.apache.org/proper/commons-codec/) and 
[byte-streams](https://github.com/ztellman/byte-streams) do exist, but they are heavy-weight 
candidates and rather grander in terms of scope. Ideally, I would like a lightweight 
(dependency-free), Clojure-native solution.
 

## Usage
The focus is on the following objects (followed by an optional map), but there are abstractions for everything.

- `java.lang.Integer`
- `java.lang.Long`
- `java.lang.Double`
- `java.math.BigInteger`
- `java.math.BigDecimal`
- `java.lang.String` -  {:encoding  (or string? charset? #{:b2 :b8 :b16 :b64 :uuid}) :b64-flavor #{:mime :url}}
- `java.util.UUID`
- `java.io.InputStream` -  {:buffer-size pos-int?}
- `java.io.File`  -  {:buffer-size pos-int?} 
- `java.net.URL`  -  {:buffer-size pos-int?}
- `java.net.URI`  -  {:buffer-size pos-int?}
- `java.awt.image.BufferedImage` - {:buffer-size pos-int? :image-type string?}
- `java.nio.ByteBuffer` 
- `java.nio.channels.ReadableByteChannel` - {:buffer-size pos-int?} 
- `java.io.Serializable`

One could argue that `java.lang.String` deserves extra attention, as it can represent various encodings (e.g. unicode, baseN, uuid etc).

### bites.core/to-bytes \[x ?opts\]
Turns any of the aforementioned classes to a byte-array. A mere wrapper around `bites.convert/toBytes`.
`opts` are optional and not even needed most of the times (see list above). For example:

```clj
(let [uid (UUID/randomUUID)] 
  (= (to-bytes uid nil)
     ;; the bytes of a UUID object match with the bytes
     ;; of the String representation of that object
     (to-bytes (str uid) {:encoding :uuid}))) ;; => true
```


### bites.core/from-bytes \[klass bs ?opts\]
The opposite of `to-bytes`. Returns an instance of `klass` given bytes `bs` and `opts`. 
A mere wrapper around `bites.convert/fromBytes`, and as such may require type-hinting at the call site.
`java.io.File`, `java.net.URL` and `java.net.URI` don't participate in this. 
Moreover,`java.io.Serializable` is hardly useful as `klass` in this context. It will do the right thing,
but you need to know the concrete type in order to do anything useful with the result, 
and if you know the actual type, then you're better off providing custom to/from impls for it. 

```clj
(let [uid (to-bytes (UUID/randomUUID) nil)] 
  (= (str (from-bytes uid nil))
     ;; the bytes of a UUID object match with the bytes
     ;; of the String representation of that object
     (from-bytes String uid {:encoding :uuid}))) ;; => true
```

### bites.core/def-from \[name doc-string klass\]
Defines a type-hinted version of `from-bytes` (according to `klass`), taking one or two args (as opposed to three).
For example:

```clj
(def-from bytes->string nil String) ;; define it

(-> (.getBytes "hi")
    (bytes->string  nil) ;; use it
    ;; no reflection 
    (.substring 0 2)) ;; => "hi"
```

## Limitations
`to-bytes` returns a **single** byte-array. This means that the usual limitations of the JVM 
with respect to array sizes apply here too. More specifically, array-indexing using 
32-bit integers means that ~2GB is the maximum possible size (i.e. don't expect to be able
to read something larger than that into a single byte-array).

See [rapio](https://github.com/jimpil/rapio) 
for reading **local** resources larger than 2GB into a sequence of byte-arrays.     

## Requirements
- Java 8+

## License

Copyright © 2020 Dimitrios Piliouras

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
