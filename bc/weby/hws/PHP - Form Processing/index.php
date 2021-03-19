<?php

require_once(__DIR__ . '/recodex_lib.php');

/*
 * Your code goes here...
 */

function validate_names(array &$data, string $key) : Bool {
    if(isset($data[$key]) && is_string($data[$key]) && $data[$key] !== '' && strlen($data[$key]) <= 100) {
        return true;
    }
    return false;
}

function validate_email(array &$data, string $key) {
    if(isset($data[$key]) && strlen($data[$key]) <= 200) {
        return filter_var($data[$key], FILTER_VALIDATE_EMAIL);
    }
    return false;
}

function validate_deliveryBoy(array &$data, string $key) {
    $deliveryBoys = array ('jesus', 'santa', 'moroz', 'hogfather', 'czpost', 'fedex');
    return in_array($data[$key], $deliveryBoys);
 }

function validate_gifts(array &$data, string $key) {
    $gifts = array('socks', 'points', 'jarnik', 'cash', 'teddy', 'other');
    if(isset($data[$key]) && is_array($data[$key])) { 
        foreach($data[$key] as $g) {
            if(!in_array($g, $gifts)) {
                return false;
            }
        }
    }
    else if(isset($data[$key]) && !is_array($data[$key])) {
        return false;
    }
    return true;
}

 function validate_unboxDay(array &$data, string $key) {
     if(isset($data[$key]) && (intval($data[$key]) == 24 || intval($data[$key]) == 25 )) {
         return true;
     }
     return false;
 }

 function validate_time(array &$data, string $key) {
     if(isset($data[$key])) {
         if($data[$key] === '') {
            return null;
         }
         if(strlen($data[$key]) <= 5 && preg_match('/^([0-9]{1,2}):([0-9]{2})$/', $data[$key], $matches)) {
             $hours = intval($matches[1]);
             $minutes = intval($matches[2]);
             if($hours <= 23 && $minutes <= 59) {
                return intval($matches[1]) * 60 + intval($matches[2]);
             }
             else {
                 return false;
             }
         }
     }
     return false;
 }

 function containts_other(array &$data) {
     return isset($data['gifts']) && is_array($data['gifts']) && in_array('other', $data['gifts']);
 }

 function validate_gift_custom(array &$data, string $key) {
    //  if(!isset($data[$key])) {
    //      return false;
    //  }
    if(containts_other($data)) {
        // if(isset($data[$key]) && $data[$key] !== '') { 
        if(validate_names($data, $key)) {
            return $data[$key];
        }
        return false;
    }
    else {
        return null;
    //     if(isset($data, $key) && $data[$key] === '') {
    //         return null;
    //     }
    //     return false;
    }
 }

function validate(array &$data) : Array {
    // unset($data['giftCustom']);
    // array_push($data['gifts'], 'ponozky');
    // unset($data['gifts']);
    // unset($data['giftCustom']);
    $output = array();
    $output['valid'] = array();
    $output['invalid'] = array();

    //firstName
    if(validate_names($data, 'firstName')) {
        $output['valid']['firstName'] = htmlspecialchars($data['firstName']);
    }
    else {
        array_push($output['invalid'], 'firstName');
    }

    //lastName
    if(validate_names($data, 'lastName')) {
        $output['valid']['lastName'] = htmlspecialchars($data['lastName']);
    }
    else {
        array_push($output['invalid'], 'lastName');
    }

    //email
    $email_checked = validate_email($data, 'email');
    if($email_checked !== false) {
        $output['valid']['email'] = $email_checked;
    }
    else {
        array_push($output['invalid'], 'email');
    }

    //deliveryBoy
    if(validate_deliveryBoy($data, 'deliveryBoy')) {
        $output['valid']['deliveryBoy'] = $data['deliveryBoy'];
    }
    else {
        array_push($output['invalid'], 'deliveryBoy');
    }

    //unboxDay
    if(validate_unboxDay($data, 'unboxDay')) {
        $output['valid']['unboxDay'] = intval($data['unboxDay']);
    }
    else {
        array_push($output['invalid'], 'unboxDay');
    }

    //fromTime
    $fromT = validate_time($data, 'fromTime');
    if($fromT !== false) {
        $output['valid']['fromTime'] = $fromT;
    }
    else {
        array_push($output['invalid'], 'fromTime');
    }

    //toTime
    $toT = validate_time($data, 'toTime');
    if($toT !== false) {
        $output['valid']['toTime'] = $toT;
    }
    else {
        array_push($output['invalid'], 'toTime');
    }

    // //toTime > fromTime
    // if(isset($output['valid']['fromTime']) && isset($output['valid']['toTime']) && 
    //    $output['valid']['fromTime'] > $output['valid']['toTime']) {
    //        unset($output['valid']['fromTime']);
    //        unset($output['valid']['toTime']);
    //        array_push($output['invalid'], 'fromTime');
    //        array_push($output['invalid'], 'toTime');
    //    }

    //gifts
    if(validate_gifts($data, 'gifts')) {
        if(isset($data['gifts'])) {
            $output['valid']['gifts'] = $data['gifts'];
        }
        else {
            $output['valid']['gifts'] = array();
        }
    }
    else {
        array_push($output['invalid'], 'gifts');
    }


    //giftCustom
    $gift_c = validate_gift_custom($data, 'giftCustom'); 
    if($gift_c !== false) {
        if($gift_c === null) {
            $output['valid']['giftCustom'] = null;
        }
        else {
            $output['valid']['giftCustom'] = htmlspecialchars($gift_c);
        }
    }
    else {
        array_push($output['invalid'], 'giftCustom');
    }
    return $output;
}

if($_SERVER['REQUEST_METHOD'] == 'GET') { //show form
    require __DIR__ . '/form_template.html';
}
else if($_SERVER['REQUEST_METHOD'] == 'POST') { //form submitted

    $output = validate($_POST);
    if(count($output['invalid']) === 0) { //valid input from client
        recodex_save_survey($output['valid']['firstName']
                          , $output['valid']['lastName']
                          , $output['valid']['email']
                          , $output['valid']['deliveryBoy']
                          , $output['valid']['unboxDay']
                          , $output['valid']['fromTime']
                          , $output['valid']['toTime']
                          , $output['valid']['gifts']
                          , $output['valid']['giftCustom']
    );
    }
    else { //invalid input from client
        recodex_survey_error('Baf!', $output['invalid']);
    }

    header('Location: ' . $_SERVER['PHP_SELF']);
}
