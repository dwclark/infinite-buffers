(in-package :infinite-buffers-test)

(def-suite test-coding)
(in-suite test-coding)

(test test-min-max
  (is (= 9223372036854775807 (ib::max-sint 8)))
  (is (= -9223372036854775808 (ib::min-sint 8)))
  (is (= (mask-field (byte 64 0) -1) (ib::max-uint 8))))

(test test-sint-uint
  (is (= 0 (ib::sint->uint 4 0)))
  (is (= 0 (ib::uint->sint 4 0)))
  (is (= (ib::max-uint 4) (ib::sint->uint 4 -1)))
  (is (= -1 (ib::uint->sint 4 (ib::max-uint 4))))
  (is (= -123456789 (ib::uint->sint 4 (ib::sint->uint 4 -123456789))))
  (is (= 123456789 (ib::uint->sint 8 (ib::sint->uint 8 123456789)))))

(test test-zigzag
  (is (= 0 (ib::sint->zigzag 0)))
  (is (= 0 (ib::zigzag->sint 0)))
  (is (= 39 (ib::sint->zigzag -20)))
  (is (= 40 (ib::sint->zigzag 20)))
  (is (= -20 (ib::zigzag->sint (ib::sint->zigzag -20))))
  (is (= (ib::max-sint 4) (ib::zigzag->sint (ib::sint->zigzag (ib::max-sint 4)))))
  (is (= (ib::min-sint 4) (ib::zigzag->sint (ib::sint->zigzag (ib::min-sint 4))))))
      
