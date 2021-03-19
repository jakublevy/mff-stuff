<?php
require_once('db.php');

/**
 * Deletes an item with item_id = $item_id from the shopping list.
 *
 * @param int $item_id Id of an item to delete.
 *
 * @return Bool Indicates whether the DB operation was successful.
 */
function delete_item_from_cart(int $item_id) : Bool {
    $stmt = $GLOBALS['_DBH']->prepare('delete from list where item_id = :itm;');
    $stmt->bindValue(':itm', $item_id, PDO::PARAM_INT);
    $stmt->execute();
    return $stmt->rowCount() === 1;
}
if($_SERVER['REQUEST_METHOD'] == 'POST' and isset($_POST['del_item_id'])) {
    if(!delete_item_from_cart($_POST['del_item_id'])) {
        http_response_code(422); //Unprocessable Entity
    }
}
?>