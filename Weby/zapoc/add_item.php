<?php
require_once('db.php');

function generate_datalist() : String {
    $out = '<datalist id="items">';
    $data = $GLOBALS['_DBH']->query('select name from items;')->fetchAll();
    foreach($data as $row) {
        $out .= "<option value=\"${row['name']}\">";
    }
    $out .= '</datalist>';
    return $out;
}

function generate_form() : String {
    $out = '<div class="form"><form action="add_item.php" method="post">' . 
                    'Item:' .
                    '<input list="items" name="item" required><br>' . 
                    'Amount:' .
                    '<input type="number" min="1" name="amount" required><br>' .
                    '<button class="btn okBtn" type="submit">Add</button>';
    $out .= generate_datalist();
    $out .= '</form></div>';
    return $out;
}

function item2id(string $item) : ?Int {
    $stmt = $GLOBALS['_DBH']->prepare('select id from items where items.name = :itm;');
    $stmt->bindValue(':itm', $item, PDO::PARAM_STR);
    $stmt->execute();
    $result = $stmt->fetch(PDO::FETCH_ASSOC); 
    if(isset($result['id'])) {
        return $result['id'];
    }
    return null;
}

function insert_item(string $item) : Int {
    $stmt = $GLOBALS['_DBH']->prepare('insert into items (name) values (:itm);');
    $stmt->bindValue(':itm', $item, PDO::PARAM_STR);
    $stmt->execute();
    return $GLOBALS['_DBH']->lastInsertId();
}

function last_position() : Int {
    $data = $GLOBALS['_DBH']->query('select max(position) from list;')->fetchAll();
    return $data[0]['max(position)'];
}

function insert_into_cart(int $id, int $amount) {
    $last_pos = last_position();
    $stmt = $GLOBALS['_DBH']->prepare('insert into list (item_id, amount, position) values (:itm, :amnt, :pos);');
    $stmt->bindValue(':itm', $id, PDO::PARAM_INT);
    $stmt->bindValue(':amnt', $amount, PDO::PARAM_INT);
    $stmt->bindValue(':pos', $last_pos + 1, PDO::PARAM_INT);
    $stmt->execute();
}

function update_on_cart(int $id, int $amount) : Bool {
    $stmt = $GLOBALS['_DBH']->prepare('update list set amount = amount + :amnt where item_id = :itm;');
    $stmt->bindValue(':itm', $id, PDO::PARAM_INT);
    $stmt->bindValue(':amnt', $amount, PDO::PARAM_INT);
    $stmt->execute();
    return $stmt->rowCount() === 1;
}

echo("<h3>Add Item</h3>");
echo(generate_form());

if ($_SERVER['REQUEST_METHOD'] === 'POST' and isset($_POST['item']) and isset($_POST['amount'])) {
    $id = item2id($_POST['item']);
    if($id === null) { //add new item do db, add it to cart
       $id = insert_item($_POST['item']);
       insert_into_cart($id, $_POST['amount']);
    }
    else { //add/update existing item on cart
        if(!update_on_cart($id, $_POST['amount'])) {
            insert_into_cart($id, $_POST['amount']);
        }
    }
header('Location: index.php');
}
?>