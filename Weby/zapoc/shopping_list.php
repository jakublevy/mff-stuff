<?php
require_once('db.php');

function get_cart() : Array {
    $data = $GLOBALS['_DBH']->query('select items.id, name, amount from list join items on items.id = item_id order by position;')->fetchAll();
    return $data;
}

function delete_btn(int $id) : String {
    return "<button class=\"btn dltBtn\" data-id=\"$id\">Delete</button>";
}

function edit_btn(int $id) : String {
    return "<button class=\"btn editBtn\" data-id=\"$id\">Edit</button>";
}

function swap_btn(int $prev_id, int $next_id) : String {
    return "<button class=\"btn swapBtn\" data-prev=\"$prev_id\" data-next=\"$next_id\" id=\"swp-$prev_id-$next_id\">↕</button>";
}

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