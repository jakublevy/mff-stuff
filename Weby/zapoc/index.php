<!DOCTYPE html>
<html lang="en">
<head>
<title>Shopping list</title>
<script src="client.js"></script>
<link rel="stylesheet" type="text/css" href="styles.css">
</head>
<body>
<div class="wrapper">
<h2>Shopping List</h2>
<?php
require_once('db.php');
include('shopping_list.php'); 
include('add_item.php');
?> 
</div>
</body>
</html>