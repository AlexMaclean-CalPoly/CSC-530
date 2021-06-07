#lang typed/racket

(struct AVS (i o) ([update : (i o -> AVS)] ([execute : (i ->  (Listof o))])))
