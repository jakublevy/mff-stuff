function overlap(r, rects) {
    const pred = (el) => doOverlap(r, el);
    return rects.some(pred);
}

function doOverlap(r1, r2) {
  return !(r2.left >= r1.left + r1.width || 
           r2.left + r2.width <= r1.left || 
           r2.top >= r1.top + r1.height ||
           r2.top + r2.height <= r1.top);
}

function area(rect) {
    return rect.width * rect.height
}

function maxFreeRect(width, height, rects) {
   let lefts = rects.map(r => r.left + r.width)
   lefts[lefts.length] = 0

   let tops = rects.map(r => r.top + r.height)
   tops[tops.length] = 0

   var maxarea = undefined
   var maxrect = undefined

   for(let li = 0; li < lefts.length; ++li) {
       for(let ti = 0; ti < tops.length; ++ti) {
           let leftBlock = rects.filter(r => r.left > lefts[li]).map(r => r.left)
           leftBlock[leftBlock.length] = width

           let topBlock = rects.filter(r => r.top > tops[ti]).map(r => r.top)
           topBlock[topBlock.length] = height

           for(let lbi = 0; lbi < leftBlock.length; ++lbi) {
               for(let tbi = 0; tbi < topBlock.length; ++tbi) {
                    let newrect = {left: lefts[li], top: tops[ti], width: leftBlock[lbi] - lefts[li], height: topBlock[tbi] - tops[ti]}
                    if(!overlap(newrect, rects)) {
                        let newarea = area(newrect)
                        if(typeof maxarea === 'undefined' || newarea > maxarea) {
                            maxarea = newarea 
                            maxrect = newrect
                        }
                    }
                }
           }

       }
   }
   return maxrect
}



var testInput = {
    "width": 600,
    "height": 400,
    "rects": [
        { "left": 20, "top": 10, "width": 150, "height": 30 },
        { "left": 500, "top": 50, "width": 40, "height": 300 },
        { "left": 290, "top": 300, "width": 20, "height": 100 },
        { "left": 50, "top": 270, "width": 80, "height": 80 }
    ]
};

// maxFreeRect(testInput.width, testInput.height, testInput.rects);

// In nodejs, this is the way how export is performed.
// In browser, module has to be a global varibale object.
module.exports = { maxFreeRect };
