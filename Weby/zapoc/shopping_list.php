<?php
require_once('db.php');

/**
 * Fetches the shopping list from DB.
 *
 * @return Array Products on the shopping list.
 */
function get_cart() : Array {
    $data = $GLOBALS['_DBH']->query('select items.id, name, amount from list join items on items.id = item_id order by position;')->fetchAll();
    return $data;
}

/**
 * Button for deleting an item from the shopping list.
 *
 * @param int $id item_id in DB
 *
 * @return String HTML button element.
 */
function delete_btn(int $id) : String {
    return "<button class=\"btn dltBtn\" data-id=\"$id\">Delete</button>";
}

/**
 * Button for editing the amount of an item on the shopping list.
 *
 * @param int $id item_id in DB.
 *
 * @return String HTML button element.
 */
function edit_btn(int $id) : String {
    return "<button class=\"btn editBtn\" data-id=\"$id\">Edit</button>";
}

/**
 * Button for swapping two adjacent rows.
 *
 * @param int $prev_id First row.
 * @param int $next_id Next adjacent row.
 *
 * @return String HTML button element.
 */
function swap_btn(int $prev_id, int $next_id) : String {
    return "<button class=\"btn swapBtn\" data-prev=\"$prev_id\" data-next=\"$next_id\" id=\"swp-$prev_id-$next_id\">↕</button>";
}

/**
 * Displays a table containing shopping list.
 *
 * @return void
 */
function display_cart() {
    $data = get_cart();
    
    echo('<table><thead><tr><th>Item</th><th>Amount</th><th></th><th></th><th></th></tr></thead>');
    $c = count($data) - 1;
    $i = 0;
    foreach($data as $row) {
        echo("<tr id=\"row-${row['id']}\">");
        if($i < $c) {
            $next_id = $data[$i+1]['id'];
            echo("<td class=\"itemCell\">${row['name']}</td><td class=\"amountCell\" id=\"act-${row['id']}\">${row['amount']}</td><td class=\"swapCell\" >" . swap_btn($row['id'], $next_id) . "</td><td class=\"btnCell\" >" . delete_btn($row['id']) . '</td><td class="btnCell" >' . edit_btn($row['id']) . '</td>');
        }
        else {
            echo("<td class=\"itemCell\">${row['name']}</td><td class=\"amountCell\" id=\"act-${row['id']}\">${row['amount']}</td><td class=\"swapCell\" ></td><td class=\"btnCell\" >" . delete_btn($row['id']) . '</td><td class="btnCell" >' . edit_btn($row['id']) . '</td>');
        }
        echo('</tr>');
        ++$i;
    }
    echo('</table>');
}
display_cart();
?>