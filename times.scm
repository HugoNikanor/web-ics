;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(use-modules (statprof))

(statprof (lambda ()
            (map 1+ (iota 1000000))
            #f))
;; %     cumulative   self             
;; time   seconds     seconds  procedure
;;  47.37 156784.16      0.22  ice-9/boot-9.scm:220:5:map1
;;  26.32      0.12      0.12  ice-9/boot-9.scm:1151:0:iota
;;  21.05      0.10      0.10  #{1+}#
;;   5.26      0.02      0.02  list?
;;   0.00      0.46      0.00  <current input>:8:10
;;   0.00      0.02      0.00  ice-9/boot-9.scm:215:2:map
;; ---
;; Sample count: 19
;; Total time: 0.464966206 seconds (1.226576068 seconds in GC)

(statprof (lambda ()
            (calendar-paths->ics-objects *cal-paths*)
            #f))
;; => #f
%     cumulative   self             
time   seconds     seconds  procedure
 18.02      1.69      0.55  ice-9/format.scm:139:4:format:format-work
 15.70      0.48      0.48  open-output-string
  9.30      2.54      0.28  ice-9/format.scm:39:0:format
  8.14      0.25      0.25  write-char
  6.98      0.21      0.21  make-string
  5.81      0.18      0.18  get-output-string
  5.81      0.18      0.18  display
  3.49      0.11      0.11  char=?
  2.91      0.71      0.09  ice-9/format.scm:800:4:format:out-obj-padded
  2.33      0.51      0.07  ice-9/ports.scm:544:0:call-with-output-string
  2.33      0.07      0.07  ics/common.scm:45:0:debug
  2.33      0.07      0.07  oop/goops.scm:1465:9
  2.33      0.07      0.07  char-upcase
  1.74      0.05      0.05  localtime
  1.16      2.57      0.04  ics/common.scm:50:0:debug-fsm
  1.16      0.04      0.04  length
  1.16      0.04      0.04  string-append
  1.16      0.04      0.04  close-port
  0.58      4.95      0.02  ics/fsm.scm:179:0:fsm-read-ics-object
  0.58      2.91      0.02  ics/fsm.scm:265:0:fsm-read-ics-stream
  0.58      0.07      0.02  ics/parser.scm:53:0
  0.58      0.04      0.02  srfi/srfi-19.scm:1216:0:integer-reader
  0.58      0.02      0.02  oop/goops.scm:1185:0:class-slot-definition
  0.58      0.02      0.02  eof-object?
  0.58      0.02      0.02  oop/goops.scm:1405:2
  0.58      0.02      0.02  ics/fsm.scm:73:0:string-append-char
  0.58      0.02      0.02  oop/goops.scm:413:0:%allocate-instance
  0.58      0.02      0.02  oop/goops.scm:724:0
  0.58      0.02      0.02  %after-gc-thunk
  0.58      0.02      0.02  oop/goops.scm:715:0
  0.58      0.02      0.02  string=?
  0.58      0.02      0.02  peek-char
  0.00     89.00      0.00  srfi/srfi-1.scm:590:5:map1
  0.00      3.03      0.00  srfi/srfi-1.scm:678:0:append-map
  0.00      3.03      0.00  <current input>:72:26
  0.00      3.03      0.00  code.scm:212:23
  0.00      2.27      0.00  ics/fsm.scm:101:0:fsm-read-property
  0.00      0.26      0.00  ice-9/boot-9.scm:220:5:map1
  0.00      0.23      0.00  ics/fsm.scm:191:9
  0.00      0.11      0.00  oop/goops.scm:2986:0:change-object-class
  0.00      0.09      0.00  ice-9/boot-9.scm:781:2:catch
  0.00      0.09      0.00  obj.scm:48:0
  0.00      0.09      0.00  srfi/srfi-19.scm:1443:0:string->date
  0.00      0.05      0.00  srfi/srfi-19.scm:605:0:local-tz-offset
  0.00      0.04      0.00  oop/goops.scm:3028:0
  0.00      0.04      0.00  srfi/srfi-19.scm:1395:0:priv:string->date
  0.00      0.02      0.00  anon #x7f81db8734c0
  0.00      0.02      0.00  ics/type/property/property.scm:65:0
  0.00      0.02      0.00  oop/goops.scm:2830:0:%initialize-object
  0.00      0.02      0.00  ics/type/content.scm:46:0
  0.00      0.02      0.00  ics/type/stream.scm:87:0
  0.00      0.02      0.00  oop/goops.scm:2990:5
  0.00      0.02      0.00  srfi/srfi-1.scm:634:2:for-each
  0.00      0.02      0.00  oop/goops.scm:2317:0:slot-init-function
---
Sample count: 172
Total time: 3.031848989 seconds (0.253282441 seconds in GC)

(define-syntax-rule (thunk body ...)
  (lambda () body ...))

(define files
  (apply append (map (cut get-files-in-dir <> "ics")
                     *cal-paths*)))

(statprof (thunk
           (map (lambda (filename)
                  (-> filename
                      open-input-file
                      ics->scm
                      car
                      ics-object-components
                      car
                      (change-class <ics-path-object>)
                      (slot-set-ret! 'path filename)
                      #; (slot-set-ret! 'calendar name)
                      ))
                files)
           #f)
          #:count-calls? #t)

%     cumulative   self             
time   seconds    seconds   calls   procedure
 12.63      1.80      1.80 1033918  char=?
  9.56      9.08      1.36   85768  ice-9/format.scm:139:4:format:format-work
  7.44      1.13      1.06  343072  display
  7.20      1.04      1.03  428840  write-char
  6.49      1.26      0.92  257304  open-output-string
  5.79      0.82      0.82  351878  memq
  4.84     11.30      0.69   85768  ice-9/format.scm:39:0:format
  4.13      0.61      0.59  257304  get-output-string
  2.72      3.28      0.39  171536  ice-9/format.scm:800:4:format:out-obj-padded
  2.72      0.39      0.39  257304  char-upcase
  2.72      0.39      0.39      35  %after-gc-thunk
  2.60      0.39      0.37  171536  make-string
  2.01      0.29      0.29  171536  ice-9/format.scm:779:46
  1.89      0.27      0.27   92792  string-append
  1.89      0.27      0.27  235963  oop/goops.scm:1465:9
  1.77      1.90      0.25  171536  ice-9/ports.scm:544:0:call-with-output-string
  1.77      0.25      0.25  171536  list-ref
  1.65      0.24      0.24  251458  anon #x7feaba860518
  1.65      0.24      0.24  174714  length
  1.53      0.22      0.22  120627  read-char
  1.53      0.22      0.22  172581  anon #x7feaba860510
  1.53      0.22      0.22   92507  string
  1.42      0.20      0.20   85768  close-port
  1.18     11.70      0.17   85768  ics/common.scm:50:0:debug-fsm
  1.18      0.17      0.17  118808  oop/goops.scm:715:0
  1.06      0.15      0.15  128966  eof-object?
  0.94      0.35      0.13   92507  ics/fsm.scm:73:0:string-append-char
  0.94      0.13      0.13   85768  port-column
  0.94      0.13      0.13     331  anon #x7feaba8604b0
  0.71      0.10      0.10   85768  ice-9/format.scm:1609:9
  0.59     11.11      0.08    3302  ics/fsm.scm:101:0:fsm-read-property
  0.59      0.50      0.08  112955  ics/parser.scm:53:0
  0.47      0.07      0.07                            oop/goops.scm:1405:2
  0.47      0.07      0.07   98331  ics/common.scm:45:0:debug
  0.35      0.05      0.05   14062  struct-ref/unboxed
  0.35      0.05      0.05   22520  oop/goops.scm:1465:9
  0.24     24.41      0.03    3671  ics/fsm.scm:179:0:fsm-read-ics-object
  0.24      0.05      0.03    3664  oop/goops.scm:2830:0:%initialize-object
  0.24      0.03      0.03   12563  ics/common.scm:55:2:debug-fsm-transition
  0.24      0.03      0.03    3115  list?
  0.12      1.08      0.02    5930  ice-9/boot-9.scm:220:5:map1
  0.12      0.13      0.02    3664  oop/goops.scm:3028:0
  0.12      0.08      0.02    4158  srfi/srfi-19.scm:1395:0:priv:string->date
  0.12      0.03      0.02    1810  oop/goops.scm:2990:5
  0.12      0.03      0.02    3845  oop/goops.scm:413:0:%allocate-instance
  0.12      0.02      0.02     502  srfi/srfi-19.scm:713:0:date->time-utc
  0.12      0.02      0.02     522  srfi/srfi-19.scm:1375:38
  0.12      0.02      0.02    1629  oop/goops.scm:1207:0:slot-set!
  0.12      0.02      0.02    3302  unread-char
  0.12      0.02      0.02    3294  assoc
  0.12      0.02      0.02    6948  srfi/srfi-19.scm:1200:0:char->int
  0.12      0.02      0.02     823  srfi/srfi-19.scm:285:0:make-time
  0.12      0.02      0.02    2370  ics/type/object.scm:83:8
  0.12      0.02      0.02   10350  equal?
  0.12      0.02      0.02                            oop/goops.scm:1388:2
  0.00   1353.26      0.00    2173  srfi/srfi-1.scm:590:5:map1
  0.00     85.50      0.00     522  ice-9/boot-9.scm:781:2:catch
  0.00     85.42      0.00                            ice-9/boot-9.scm:1693:5
  0.00     85.42      0.00                            ice-9/boot-9.scm:1689:0:%start-stack
  0.00     56.95      0.00                            system/vm/trap-state.scm:169:0:with-default-trap-handler
  0.00     28.47      0.00                            ice-9/ports.scm:475:0:with-error-to-port
  0.00     28.47      0.00                            ice-9/ports.scm:467:0:with-input-from-port
  0.00     28.47      0.00     160  ice-9/boot-9.scm:727:2:dispatch-exception
  0.00     28.47      0.00                            system/repl/repl.scm:202:32
  0.00     28.47      0.00                            system/repl/repl.scm:138:0:start-repl*
  0.00     28.47      0.00                            system/repl/repl.scm:156:0:run-repl*
  0.00     28.47      0.00                            statprof.scm:868:0:statprof
  0.00     28.47      0.00                            ice-9/ports.scm:471:0:with-output-to-port
  0.00     28.47      0.00                            system/repl/repl.scm:191:18
  0.00     28.47      0.00       1  statprof.scm:863:0:call-thunk
  0.00     14.24      0.00                            <current input>:55:16
  0.00     14.24      0.00                            <current input>:68:10
  0.00     14.24      0.00     181  <current input>:120:16
  0.00     14.24      0.00                            <current input>:69:16
  0.00     14.24      0.00       1  <current input>:119:10
  0.00     14.24      0.00                            <current input>:54:26
  0.00     14.05      0.00     362  ics/fsm.scm:265:0:fsm-read-ics-stream
  0.00      0.92      0.00    3178  ics/fsm.scm:191:9
  0.00      0.39      0.00                            anon #x7feaba8604c0
  0.00      0.18      0.00     181  oop/goops.scm:2986:0:change-object-class
  0.00      0.15      0.00     181  obj.scm:48:0
  0.00      0.10      0.00    5228  ics/fsm.scm:173:0:fsm-skip-property
  0.00      0.08      0.00     522  srfi/srfi-19.scm:1443:0:string->date
  0.00      0.05      0.00    3302  ics/type/content.scm:46:0
  0.00      0.05      0.00    2752  ics/type/property/property.scm:65:0
  0.00      0.03      0.00    2752  ice-9/boot-9.scm:215:2:map
  0.00      0.03      0.00     181  srfi/srfi-1.scm:634:2:for-each
  0.00      0.03      0.00    2952  srfi/srfi-19.scm:1216:0:integer-reader
  0.00      0.02      0.00    1267  oop/goops.scm:2317:0:slot-init-function
  0.00      0.02      0.00    1810  oop/goops.scm:1185:0:class-slot-definition
  0.00      0.02      0.00     362  obj.scm:92:0
  0.00      0.02      0.00     181  srfi/srfi-19.scm:423:0:time-difference
  0.00      0.02      0.00     362  find
  0.00      0.00      0.00    5930  string-split
  0.00      0.00      0.00     362  time.scm:44:2
  0.00      0.00      0.00    3845  %clear-fields!
  0.00      0.00      0.00     160  ice-9/boot-9.scm:769:2:throw
  0.00      0.00      0.00    1004  abs
  0.00      0.00      0.00     160  anon #x7feaba860500
  0.00      0.00      0.00     522  ice-9/boot-9.scm:693:2:make-prompt-tag
  0.00      0.00      0.00     522  srfi/srfi-19.scm:1363:38
  0.00      0.00      0.00       1  set-vm-trace-level!
  0.00      0.00      0.00     182  srfi/srfi-1.scm:586:2:map
  0.00      0.00      0.00     222  srfi/srfi-19.scm:1258:0:zone-reader
  0.00      0.00      0.00     462  srfi/srfi-19.scm:1377:38
  0.00      0.00      0.00     296  statprof.scm:263:4:profile-signal-handler
  0.00      0.00      0.00   12552  char-numeric?
  0.00      0.00      0.00     160  fluid-ref*
  0.00      0.00      0.00     543  oop/goops.scm:1220:0:slot-bound?
  0.00      0.00      0.00     280  srfi/srfi-19.scm:605:0:local-tz-offset
  0.00      0.00      0.00     462  srfi/srfi-19.scm:1380:38
  0.00      0.00      0.00       1  vm-trace-level
  0.00      0.00      0.00    1004  srfi/srfi-19.scm:265:0:time-normalize!
  0.00      0.00      0.00     140  time.scm:45:3
  0.00      0.00      0.00     181  %modify-instance
  0.00      0.00      0.00     181  ics.scm:59:0:ics->scm
  0.00      0.00      0.00     181  srfi/srfi-19.scm:415:0:time-difference!
  0.00      0.00      0.00    1810  oop/goops.scm:458:0:slot-definition-name
  0.00      0.00      0.00     181  ics/type/stream.scm:87:0
  0.00      0.00      0.00     362  oop/goops.scm:1465:9
  0.00      0.00      0.00     296  ice-9/eval.scm:191:12
  0.00      0.00      0.00     222  srfi/srfi-19.scm:1392:23
  0.00      0.00      0.00     181  srfi/srfi-19.scm:256:0:copy-time
  0.00      0.00      0.00     362  obj.scm:92:0:extract
  0.00      0.00      0.00      20  time.scm:45:3
  0.00      0.00      0.00     181  oop/goops.scm:724:0
  0.00      0.00      0.00     885  procedure?
  0.00      0.00      0.00     296  ice-9/eval.scm:590:16
  0.00      0.00      0.00    2933  oop/goops.scm:724:0
  0.00      0.00      0.00     181  oop/goops.scm:724:0
  0.00      0.00      0.00     296  ice-9/eval.scm:330:13
  0.00      0.00      0.00     181  ics/type/stream.scm:68:0:ics-read
  0.00      0.00      0.00    3483  oop/goops.scm:724:0
  0.00      0.00      0.00    3302  oop/goops.scm:1465:9
  0.00      0.00      0.00     280  ice-9/boot-9.scm:1555:0:tm:gmtoff
  0.00      0.00      0.00     181  ics/fsm.scm:67:0:ics-calendar-object?
  0.00      0.00      0.00    1810  oop/goops.scm:1232:0:slot-exists?
  0.00      0.00      0.00     592  ice-9/eval.scm:282:4
  0.00      0.00      0.00     181  open-file
  0.00      0.00      0.00    2952  srfi/srfi-19.scm:1227:2
  0.00      0.00      0.00     362  oop/goops.scm:721:0
  0.00      0.00      0.00     296  statprof.scm:219:0:existing-profiler-state
  0.00      0.00      0.00    3664  oop/goops.scm:2874:0
  0.00      0.00      0.00     181  ics/type/stream.scm:72:0
  0.00      0.00      0.00     181  oop/goops.scm:3016:0
  0.00      0.00      0.00     543  oop/goops.scm:1192:0:slot-ref
  0.00      0.00      0.00     181  oop/goops.scm:724:0
  0.00      0.00      0.00     181  port?
  0.00      0.00      0.00     725  anon #x7feaba86050c
  0.00      0.00      0.00    3845  oop/goops.scm:724:0
  0.00      0.00      0.00     502  srfi/srfi-19.scm:573:0:encode-julian-day-number
  0.00      0.00      0.00    7335  string=?
  0.00      0.00      0.00       1  statprof.scm:333:0:statprof-stop
  0.00      0.00      0.00     181  oop/goops.scm:724:0
  0.00      0.00      0.00     160  srfi/srfi-19.scm:167:0:time-error
  0.00      0.00      0.00    3302  ics/parser.scm:56:0
  0.00      0.00      0.00     362  time.scm:43:0:string->date*
  0.00      0.00      0.00   13194  peek-char
  0.00      0.00      0.00     522  open-input-string
  0.00      0.00      0.00     181  ice-9/ports.scm:406:0:open-input-file
  0.00      0.00      0.00    2752  oop/goops.scm:461:0:slot-definition-allocation
  0.00      0.00      0.00    3114  oop/goops.scm:561:42
  0.00      0.00      0.00     222  srfi/srfi-19.scm:1387:15
  0.00      0.00      0.00     905  oop/goops.scm:721:0
  0.00      0.00      0.00     426  string->symbol
  0.00      0.00      0.00     522  srfi/srfi-19.scm:1385:38
  0.00      0.00      0.00    3852  ics/fsm.scm:59:0:ics-token-begin?
  0.00      0.00      0.00     181  oop/goops.scm:359:0:class-slots
  0.00      0.00      0.00      20  time.scm:47:3
  0.00      0.00      0.00     140  time.scm:46:3
  0.00      0.00      0.00     502  inexact->exact
  0.00      0.00      0.00     362  ics/type/object.scm:79:0
  0.00      0.00      0.00     181  oop/goops.scm:724:0
  0.00      0.00      0.00    3302  ics/fsm.scm:63:0:ics-token-end?
  0.00      0.00      0.00    3845  oop/goops.scm:724:0
  0.00      0.00      0.00    3845  oop/goops.scm:3025:0
  0.00      0.00      0.00     280  localtime
  0.00      0.00      0.00     181  obj.scm:86:0:slot-set-ret!
  0.00      0.00      0.00     462  srfi/srfi-19.scm:1371:38
  0.00      0.00      0.00    3657  oop/goops.scm:721:0
---
Sample count: 847
Total time: 14.236727272 seconds (0.917457385 seconds in GC)

(statprof (thunk
           (map (lambda (filename)
                  (-> filename
                      open-input-file
                      ics->scm
                      #; car
                      #; ics-object-components
                      #; car
                      #; (change-class <ics-path-object>)
                      #; (slot-set-ret! 'path filename)
                      #; (slot-set-ret! 'calendar name)
                      ))
                files)
           #f)
          #:count-calls? #t)

%     cumulative   self             
time   seconds    seconds   calls   procedure
 16.95      2.32      2.32 1029216  char=?
  9.56      9.05      1.31   85768  ice-9/format.scm:139:4:format:format-work
  6.90      0.96      0.94  343072  display
  6.78      0.93      0.93  428840  write-char
  6.54      0.98      0.90  257304  open-output-string
  5.08      0.70      0.70  257304  get-output-string
  4.72     11.12      0.65   85768  ice-9/format.scm:39:0:format
  4.48      0.61      0.61  351878  memq
  4.24      0.58      0.58  257304  char-upcase
  2.42      0.38      0.33  171536  make-string
  2.42      0.33      0.33  232688  oop/goops.scm:1465:9
  2.42      0.33      0.33  174714  length
  2.30      2.77      0.31  171536  ice-9/format.scm:800:4:format:out-obj-padded
  2.30      0.31      0.31  171536  list-ref
  1.82      0.25      0.25  111874  eof-object?
  1.69      0.23      0.23  171537  anon #x7feaba860510
  1.69      0.23      0.23   85768  close-port
  1.57      0.22      0.22  112955  read-char
  1.45      0.20      0.20   92507  string
  1.33      1.44      0.18  171536  ice-9/ports.scm:544:0:call-with-output-string
  1.21     11.56      0.17   85768  ics/common.scm:50:0:debug-fsm
  1.21      0.17      0.17  247227  anon #x7feaba860518
  1.21      0.17      0.17   98331  ics/common.scm:45:0:debug
  1.09      0.15      0.15   85768  port-column
  1.09      0.15      0.15   92792  string-append
  1.09      0.15      0.15   85768  ice-9/format.scm:1609:9
  0.97      0.13      0.13  171536  ice-9/format.scm:779:46
  0.85      0.41      0.12  112955  ics/parser.scm:53:0
  0.85      0.12      0.12      16  %after-gc-thunk
  0.73      0.10      0.10     261  anon #x7feaba8604b0
  0.48      0.27      0.07   92507  ics/fsm.scm:73:0:string-append-char
  0.48      0.07      0.07  116257  oop/goops.scm:715:0
  0.36      0.05      0.05                            oop/goops.scm:1405:2
  0.24     23.69      0.03    3671  ics/fsm.scm:179:0:fsm-read-ics-object
  0.24      1.03      0.03    5930  ice-9/boot-9.scm:220:5:map1
  0.24      0.03      0.03   12563  ics/common.scm:55:2:debug-fsm-transition
  0.24      0.03      0.03   10080  struct-ref/unboxed
  0.12     10.98      0.02    3302  ics/fsm.scm:101:0:fsm-read-property
  0.12      0.07      0.02    2752  ics/type/property/property.scm:65:0
  0.12      0.02      0.02    3664  %clear-fields!
  0.12      0.02      0.02    3664  oop/goops.scm:2874:0
  0.12      0.02      0.02                            oop/goops.scm:1388:2
  0.12      0.02      0.02    3852  ics/fsm.scm:59:0:ics-token-begin?
  0.00   1313.24      0.00     182  srfi/srfi-1.scm:590:5:map1
  0.00     82.16      0.00                            ice-9/boot-9.scm:1693:5
  0.00     82.16      0.00                            ice-9/boot-9.scm:1689:0:%start-stack
  0.00     82.16      0.00                            ice-9/boot-9.scm:781:2:catch
  0.00     54.78      0.00                            system/vm/trap-state.scm:169:0:with-default-trap-handler
  0.00     27.39      0.00                            ice-9/ports.scm:475:0:with-error-to-port
  0.00     27.39      0.00                            ice-9/ports.scm:467:0:with-input-from-port
  0.00     27.39      0.00                            ice-9/boot-9.scm:727:2:dispatch-exception
  0.00     27.39      0.00                            system/repl/repl.scm:202:32
  0.00     27.39      0.00                            system/repl/repl.scm:138:0:start-repl*
  0.00     27.39      0.00                            system/repl/repl.scm:156:0:run-repl*
  0.00     27.39      0.00                            statprof.scm:868:0:statprof
  0.00     27.39      0.00                            ice-9/ports.scm:471:0:with-output-to-port
  0.00     27.39      0.00                            system/repl/repl.scm:191:18
  0.00     27.39      0.00       1  statprof.scm:863:0:call-thunk
  0.00     13.69      0.00                            <current input>:55:16
  0.00     13.69      0.00                            <current input>:68:10
  0.00     13.69      0.00     362  ics/fsm.scm:265:0:fsm-read-ics-stream
  0.00     13.69      0.00                            <current input>:69:16
  0.00     13.69      0.00       1  <current input>:176:10
  0.00     13.69      0.00                            <current input>:54:26
  0.00      0.85      0.00    3178  ics/fsm.scm:191:9
  0.00      0.12      0.00                            anon #x7feaba8604c0
  0.00      0.10      0.00    3664  oop/goops.scm:3028:0
  0.00      0.05      0.00    3302  ics/type/content.scm:46:0
  0.00      0.03      0.00    5228  ics/fsm.scm:173:0:fsm-skip-property
  0.00      0.03      0.00    3664  oop/goops.scm:413:0:%allocate-instance
  0.00      0.02      0.00    3664  oop/goops.scm:2830:0:%initialize-object
  0.00      0.00      0.00    5930  string-split
  0.00      0.00      0.00    2752  ice-9/boot-9.scm:215:2:map
  0.00      0.00      0.00       1  set-vm-trace-level!
  0.00      0.00      0.00       1  srfi/srfi-1.scm:586:2:map
  0.00      0.00      0.00     245  statprof.scm:263:4:profile-signal-handler
  0.00      0.00      0.00       1  vm-trace-level
  0.00      0.00      0.00     181  ics.scm:59:0:ics->scm
  0.00      0.00      0.00     181  ics/type/stream.scm:87:0
  0.00      0.00      0.00     245  ice-9/eval.scm:191:12
  0.00      0.00      0.00     362  oop/goops.scm:1465:9
  0.00      0.00      0.00    3302  unread-char
  0.00      0.00      0.00       1  procedure?
  0.00      0.00      0.00     245  ice-9/eval.scm:590:16
  0.00      0.00      0.00    2752  oop/goops.scm:724:0
  0.00      0.00      0.00     245  ice-9/eval.scm:330:13
  0.00      0.00      0.00     181  ics/type/stream.scm:68:0:ics-read
  0.00      0.00      0.00    3302  oop/goops.scm:724:0
  0.00      0.00      0.00    3302  oop/goops.scm:1465:9
  0.00      0.00      0.00     181  ics/fsm.scm:67:0:ics-calendar-object?
  0.00      0.00      0.00     181  <current input>:177:16
  0.00      0.00      0.00     490  ice-9/eval.scm:282:4
  0.00      0.00      0.00     181  open-file
  0.00      0.00      0.00     181  oop/goops.scm:721:0
  0.00      0.00      0.00     245  statprof.scm:219:0:existing-profiler-state
  0.00      0.00      0.00     181  ics/type/stream.scm:72:0
  0.00      0.00      0.00    2753  list?
  0.00      0.00      0.00     181  port?
  0.00      0.00      0.00       1  anon #x7feaba86050c
  0.00      0.00      0.00    7980  equal?
  0.00      0.00      0.00    3664  oop/goops.scm:724:0
  0.00      0.00      0.00    7335  string=?
  0.00      0.00      0.00       1  statprof.scm:333:0:statprof-stop
  0.00      0.00      0.00    3302  ics/parser.scm:56:0
  0.00      0.00      0.00     181  ice-9/ports.scm:406:0:open-input-file
  0.00      0.00      0.00    2752  oop/goops.scm:461:0:slot-definition-allocation
  0.00      0.00      0.00    2752  oop/goops.scm:561:42
  0.00      0.00      0.00     426  string->symbol
  0.00      0.00      0.00    3302  ics/fsm.scm:63:0:ics-token-end?
  0.00      0.00      0.00    3664  oop/goops.scm:724:0
  0.00      0.00      0.00    3664  oop/goops.scm:3025:0
  0.00      0.00      0.00   21615  oop/goops.scm:1465:9
  0.00      0.00      0.00    2933  oop/goops.scm:721:0
---
Sample count: 826
Total time: 13.694089756 seconds (0.544483808 seconds in GC)

(map (lambda (filename)
        (-> filename
            open-input-file
            close-port
                      #; ics->scm
                      #; car
                      #; ics-object-components
                      #; car
                      #; (change-class <ics-path-object>)
                      #; (slot-set-ret! 'path filename)
                      #; (slot-set-ret! 'calendar name)
            ))
      files)


(let* ((ports (map open-input-file files))
       (ics-objs (map ics->scm ports)))
  (for-each close-port ports)
  ics-objs
  #f)

                          #; ics->scm
                          #; close-port
                          #; car
                          #; ics-object-components
                          #; car
                          #; (change-class <ics-path-object>)
                          #; (slot-set-ret! 'path filename)
                          #; (slot-set-ret! 'calendar name)


(define str
  (string-join (map (cut call-with-input-file <>
                         read-string)
                    files)
               "\r\n\r\n"))

(define icss (ics-string->scm str))

(let* ((ports (map open-input-file files)))
  ()
  (ics-objs (map ics->scm ports))
  (for-each close-port ports)
  ics-objs
  #f)

(string-take str 1000)
