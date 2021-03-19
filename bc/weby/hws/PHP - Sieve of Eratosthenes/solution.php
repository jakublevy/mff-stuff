<?php

/*
 * Create an array primes from 2 to $max.
 */

function sieve($max) {
    $lp = array_fill_keys(range(2, $max+1), 0);
    $pr = array();
    for($i = 2; $i <= $max; ++$i) {
        if($lp[$i] == 0) {
            $lp[$i] = $i;
            array_push($pr, $i);
        }
        for($j = 0; $j < count($pr) && $pr[$j] <= $lp[$i] and $i * $pr[$j] <= $max; ++$j) {
            $lp[$i * $pr[$j]] = $pr[$j];
        }
    }
    return $pr;
}