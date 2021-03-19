<?php
require_once('db.php');

/**
 * Swaps the order of two items with given Ids.
 *
 * @param int $prev_id Id of an item to swap
 * @param int $next_id Id of an item to swap 
 * (Assuming that $prev_id !== $next_id)
 *
 * @return Bool Indicates whether the DB operation was successful.
 */
function swap_order(int $prev_id, int $next_id) : Bool {
    $stmt = $GLOBALS['_DBH']->prepare('update list a inner join list b on a.item_id <> b.item_id set a.position = b.position where a.item_id in (:pa, :na) and b.item_id in (:pb, :nb);');
    $stmt->bindValue(':pa', $prev_id, PDO::PARAM_INT);
    $stmt->bindValue(':na', $next_id, PDO::PARAM_INT);
    $stmt->bindValue(':pb', $prev_id, PDO::PARAM_INT);
    $stmt->bindValue(':nb', $next_id, PDO::PARAM_INT);
    $stmt->execute();
    $v = $stmt->rowCount();
    return $stmt->rowCount() === 2;
}

if($_SERVER['REQUEST_METHOD'] == 'POST' and isset($_POST['prev_id']) and isset($_POST['next_id'])) {
    if(!swap_order($_POST['prev_id'], $_POST['next_id'])) {
        http_response_code(422); //Unprocessable Entity
    }
}
?>