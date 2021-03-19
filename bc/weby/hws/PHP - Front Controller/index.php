<?php

function page_valid_chars(string $str) : Bool {
    return preg_match('#[a-zA-Z/]+#', $str);
}

function page_valid_format(string $str) : Bool {
    $no_trailing_slashes = preg_match('#^[^/][a-zA-Z/]+[^/]$#', $str);
    return !strpos($str, '//') and $no_trailing_slashes;
}

function page_valid(string $str) : Bool {
    return page_valid_chars($str) and page_valid_format($str);
}

function is_page_valid_file_u(string $str) : Bool {
    return file_exists(construct_page_file($str));
}

function is_page_valid_dir_u(string $str) : Bool {
     return is_dir(__DIR__ . '/templates/' . $str);
}

function construct_page_file_from_dir(string $str) : String {
    return construct_page_file($str) . '/index.php';
}

function construct_page_file(string $str) : String {
    return __DIR__ . '/templates/' . $str . '.php';
}

function construct_descriptor_file(string $str) : String {
    return __DIR__ . '/parameters/' . $str . '.php';
}

function header_file() : String {
    return __DIR__ . '/templates/_header.php';
}

function footer_file() : String {
    return __DIR__ . '/templates/_footer.php';
}

function checked_load(string $page_file, string $desc_file) {
    $desc = require($desc_file);

    $ok = true;
    foreach($desc as $param => $type) {
        if(isset($_GET[$param])) {
            if(((is_array($type) and in_array($_GET[$param], $type))) or $type === 'string') {
                $$param = htmlspecialchars($_GET[$param]);
            }
            else if ($type === 'int' and is_numeric($_GET[$param])) {
                $$param = intval($_GET[$param]);
            }
            else {
                $ok = false;
                http_response_code(400);
                break;
            }
        }
        else {
            $ok = false;
            http_response_code(400);
            break;
        }
    }
    if($ok) {
        try {
            include(header_file());
            include($page_file);
            include(footer_file());
        }
        catch(exception $e) {
            http_response_code(500);
        }
    }
}

function unchecked_load(string $page_file) {
    try {
        include(header_file());
        include($page_file);
        include(footer_file());
    }
    catch(exception $e) {
        http_response_code(500);
    }
}

if(isset($_GET['page'])) { 
    $page = $_GET['page'];
    if(page_valid($page)) {
        if(is_page_valid_dir_u($page)) {
            $page .= '/index';
        }
        if(is_page_valid_file_u($page)) {
            $desc_file = construct_descriptor_file($page);
            if(file_exists($desc_file)) { //check parameters
                checked_load(construct_page_file($page), $desc_file);

            }
            else { //template does not require any params
                unchecked_load(construct_page_file($page));
            }
        }
        else { 
            http_response_code(404);
        }
    }
    else {
        http_response_code(400);
    }
}
else { //page parameter missing
    http_response_code(404);
}
?>
