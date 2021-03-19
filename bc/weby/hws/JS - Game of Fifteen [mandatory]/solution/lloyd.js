/*
 * Your code goes here.
 * Do not forget that this code may be executed before the whole HTML is parsed
 * (i.e., the DOM structure may not be yet complete).
 */

/*
 * You may use the following initial position if you do not implement randomization.
 *  3  11   2   5
 *  1  13   6   8
 *  4   9      10
 * 14  12   7  15
 */

 //https://stackoverflow.com/questions/2450954/how-to-randomize-shuffle-a-javascript-array
 function shuffleArray(array) {
    for (let i = array.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1));
        [array[i], array[j]] = [array[j], array[i]];
    }
}

 function removeAllChildren(node) {
    //faster way than doing:
    //  node.innerHTML = ''
     while (node.firstChild) 
        node.removeChild(node.firstChild);
 }

 function shuffledTiles() {
    let tiles = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,'blank']
    shuffleArray(tiles)
    return tiles
 }

 //calculates the number of inversions on the game board
 function inversions(tiles) {
     let inversionsCount = 0
     for(let i = 0; i < tiles.length - 1; ++i) {
         for(let j = i + 1; j < tiles.length; ++j) {
            if(typeof tiles[i] === 'number' && typeof tiles[j] === 'number' && tiles[i] > tiles[j])
                ++inversionsCount
         }
     }
     return inversionsCount
 }

 //returns the tile index
 function tileIndex(tiles, tile) {
     for(let i = 0; i < tiles.length; ++i) {
         if(tiles[i] === tile) 
             return i
     }
     return -1
 }

 //returns boolean indicating whether a given tile is on even line
 function tileOnEvenLine(tiles, tile) {
     let tileIdx = tileIndex(tiles, tile)
     let line = Math.floor(tileIdx/4);
     return even(line)
 }

 function even(i) { return i % 2 === 0 }

 function createTileElement(value, idx) {
     let el = document.createElement('div')
     el.innerText = value
     el.id = 'game-tile-' + idx
     el.classList.add('game-tile')
     el.setAttribute('data-id', idx)
     return el
 }

 //is game currently in end position?
 function endPosition(tiles) {
    if(typeof tiles === 'undefined') {
        let board = document.getElementById('game-board')
        for(let i = 0; i < board.children.length; ++i) {
            let dataId = board.children[i].getAttribute('data-id')
            if(dataId !== board.children[i].innerText) {
                return false
            }
        }
        return true
        }
    for(let i = 0; i < tiles.length - 1; ++i) {
        if(tiles[i] !== i)
            return false
    }
    return tiles[tiles.length - 1] === 'blank'
 }

 function connectClickEvent(tileEl) {
     tileEl.addEventListener('click', tileClicked, false)
 }

 function tileClicked() {
     let board = document.getElementById('game-board')

     //1-16
     let id = parseInt(event.target.getAttribute('data-id'))

     //tile is not the right most
     if(id % 4 !== 0) {
        let nextId = 'game-tile-' + (id + 1)
        let nextEl = board.children[nextId]
        if(typeof nextEl === 'undefined')
            var newId = id + 1
     }

     //tile is not the left most
     if(id % 4 !== 1) {
         let prevId = 'game-tile-' + (id - 1)
         let prevEl = board.children[prevId]
         if(typeof prevEl === 'undefined')
            var newId = id - 1
    }

     //tile is not the top most
     if(id - 4 >= 1) {
         let topId = 'game-tile-' + (id - 4)
         let topEl = board.children[topId]
         if(typeof topEl === 'undefined') {
            var newId = id - 4
         }
    }

     //tile is not the bottom most
     if(id + 4 <= 16) {
         let bottomId = 'game-tile-' + (id + 4)
         let bottomEl = board.children[bottomId]
         if(typeof bottomEl === 'undefined') {
             var newId = id + 4
         }
     }

     //is there adjacent blank tile?
     if(typeof newId !== 'undefined') {
         event.target.id = 'game-tile-' + newId
         event.target.setAttribute('data-id', newId)
     }
     if(endPosition()) {
         event.target.addEventListener('transitionend', wonAlert);
     }
 }

 function wonAlert() {
     event.target.removeEventListener('transitionend', wonAlert, false)
     alert('You have won!')
 }

window.onload = function() {
    let board = document.getElementById('game-board')
    removeAllChildren(board)
    let tiles = shuffledTiles()
    let inversionsCount = inversions(tiles)

    //in order for 15 puzzle to be solvable exactly one condition must be met:
        //1. the number of inversions is even and blank tile is on even line
        //2. the number of inversions is odd and blank tile is on odd line
    while((even(inversionsCount) && tileOnEvenLine(tiles, 'blank')) 
      || (!even(inversionsCount) && !tileOnEvenLine(tiles, 'blank'))
      || endPosition(tiles)) {
        tiles = shuffledTiles()
        inversionsCount = inversions(tiles)
    }
    for(let i = 0; i < tiles.length; ++i) {
        if(typeof tiles[i] === 'number') {
            let tileEl = createTileElement(tiles[i], i+1)
            connectClickEvent(tileEl)
            board.appendChild(tileEl)
        }
    }
 }
