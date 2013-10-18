(require 'transpose-frame)

;;; http://www.emacswiki.org/emacs/TransposeFrame

;; Commands

;; "transpose-frame" : Swap x-direction and y-direction

;;        +------------+------------+      +----------------+--------+
;;        |            |     B      |      |        A       |        |
;;        |     A      +------------+      |                |        |
;;        |            |     C      |  =>  +--------+-------+   D    |
;;        +------------+------------+      |   B    |   C   |        |
;;        |            D            |      |        |       |        |
;;        +-------------------------+      +--------+-------+--------+

;; "flip-frame" : Flip vertically

;;        +------------+------------+      +------------+------------+
;;        |            |     B      |      |            D            |
;;        |     A      +------------+      +------------+------------+
;;        |            |     C      |  =>  |            |     C      |
;;        +------------+------------+      |     A      +------------+
;;        |            D            |      |            |     B      |
;;        +-------------------------+      +------------+------------+

;; "flop-frame" : Flop horizontally

;;        +------------+------------+      +------------+------------+
;;        |            |     B      |      |     B      |            |
;;        |     A      +------------+      +------------+     A      |
;;        |            |     C      |  =>  |     C      |            |
;;        +------------+------------+      +------------+------------+
;;        |            D            |      |            D            |
;;        +-------------------------+      +-------------------------+

;; "rotate-frame" : Rotate 180 degrees

;;        +------------+------------+      +-------------------------+
;;        |            |     B      |      |            D            |
;;        |     A      +------------+      +------------+------------+
;;        |            |     C      |  =>  |     C      |            |
;;        +------------+------------+      +------------+     A      |
;;        |            D            |      |     B      |            |
;;        +-------------------------+      +------------+------------+

;; "rotate-frame-clockwise" : Rotate 90 degrees clockwise

;;        +------------+------------+      +-------+-----------------+
;;        |            |     B      |      |       |        A        |
;;        |     A      +------------+      |       |                 |
;;        |            |     C      |  =>  |   D   +--------+--------+
;;        +------------+------------+      |       |   B    |   C    |
;;        |            D            |      |       |        |        |
;;        +-------------------------+      +-------+--------+--------+

;; "rotate-frame-anti-clockwise" : Rotate 90 degrees anti-clockwise

;;        +------------+------------+      +--------+--------+-------+
;;        |            |     B      |      |   B    |   C    |       |
;;        |     A      +------------+      |        |        |       |
;;        |            |     C      |  =>  +--------+--------+   D   |
;;        +------------+------------+      |        A        |       |
;;        |            D            |      |                 |       |
;;        +-------------------------+      +-----------------+-------+
