#lang racket

(provide complete-sketch)

(require "types.rkt")

;;
(define (complete-sketch student teacher)
  (string-append
   (format shared-sketch-code (string-join (map (λ (var) (format "if(~a) totalCost++;" var))
                                                (Sketch-vars student)) "\n    "))
   (top-print student) "\n\n" (top-print teacher)))

;;
(define (top-print sk)
  (string-append
   (string-join (map (λ (v) (format "bit ~a = 0;\n" v)) (Sketch-vars sk)) "")
   (string-join (map print-sketch-function (Sketch-funs sk)) "\n\n")))

;;
(define (print-sketch-function f)
  (format "MultiType ~a(~a) {\n~a\n}"
          (FunSK-name f)
          (string-join (map (λ (arg) (format "MultiType ~a" arg)) (FunSK-params f)) ", ")
          (print-sketch (FunSK-body f))))

;; Given a node in a sketch AST returns a string representation in the sketch syntax
(define (print-sketch s)
  (match s
    [(BinOp a op b)
     (format "binOpMT(~a, ~a, ~a)" (print-sketch a) (print-sketch b) (print-sketch op))]
    [(Return var) (format "return ~a;" (print-sketch var))]
    [(If f t e)
     (format "if (~a) {\n~a\n} else {\n~a\n}" (print-sketch f) (print-sketch t) (print-sketch e))]
    [(Assign var val) (format "MultiType ~a = ~a;" (print-sketch var) (print-sketch val))]
    [(Assign-Weight var) (format "~a = 1;" var)]
    [(cons s0 s1) (string-append (print-sketch s0) "\n" (print-sketch s1))]
    [(CallSK fun args) (format "~a(~a)" fun (string-join (map ~a args) ", "))]
    [(? boolean?) (format "new MultiType(bval=~a, flag=BOOLEAN)" (if s 1 0))]
    [(? integer?) (format "new MultiType(val=~a, flag=INTEGER)" s)]
    [(Op o) (hash-ref op-constants o)]
    [(? symbol?) s]))

;;
(define op-constants #hash((+ . ADD_OP) (- . SUB_OP) (* . MULT_OP) (/ . DIV_OP)))

;;
(define shared-sketch-code #<<SK

int INTEGER = 0;
int BOOLEAN = 1;

MultiType ADD_OP = new MultiType(val=0, flag=INTEGER);
MultiType SUB_OP = new MultiType(val=1, flag=INTEGER);
MultiType DIV_OP = new MultiType(val=2, flag=INTEGER);
MultiType MULT_OP = new MultiType(val=3, flag=INTEGER);

struct MultiType {
    int val;
    int flag;
    bit bval;
}

MultiType binOpMT(MultiType a, MultiType b, MultiType op) {
    if (op == ADD_OP)
        return addMT(a, b);
    if (op == SUB_OP)
        return subMT(a, b);
    if (op == MULT_OP)
        return multMT(a, b);
    if (op == DIV_OP)
        return divMT(a, b);
}

MultiType addMT(MultiType a, MultiType b) {
    assert a.flag == b.flag;
    if(a.flag == INTEGER)
        return new MultiType(val=a.val+b.val, flag = INTEGER);
}

MultiType subMT(MultiType a, MultiType b) {
    assert a.flag == b.flag;
    if(a.flag == INTEGER)
        return new MultiType(val=a.val-b.val, flag = INTEGER);
}

MultiType multMT(MultiType a, MultiType b) {
    assert a.flag == b.flag;
    if(a.flag == INTEGER)
        return new MultiType(val=a.val*b.val, flag = INTEGER);
}

MultiType divMT(MultiType a, MultiType b) {
    assert a.flag == b.flag;
    if(a.flag == INTEGER)
        return new MultiType(val=a.val/b.val, flag = INTEGER);
}

bit MTEquals(MultiType a, MultiType b) {
    if (a.flag == b.flag && a.flag == INTEGER)
        return a.val == b.val;
    if (a.flag == b.flag && a.flag == BOOLEAN)
        return a.bval == b.bval;

    return 0;
}

harness void main() {
    int totalCost = 0;

    MultiType result1 = teacher();
    MultiType result2 = student();

    ~a

    assert MTEquals(result1, result2);
    minimize(totalCost);
}

SK
  )
