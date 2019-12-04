<?php
include('db_config.php');

try {
    $dbh = new PDO("mysql:host=${db_config['server']};dbname=${db_config['database']}", $db_config['login'], $db_config['password']);
    $dbh->setAttribute(PDO::ATTR_EMULATE_PREPARES, true);  //make sure that PDO::lastInsertId works
    $GLOBALS['_DBH'] = $dbh;
}
catch (PDOException $e) {
    die("Could not connect to the DB.");
}
?>