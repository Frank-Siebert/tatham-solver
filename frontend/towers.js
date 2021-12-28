var example = '{ "size": 6, '+
                '"top": " 12345", '+
				'"left": "  2   ", '+
				'"right": "  4   ", '+
				'"bottom": "     2", '+
				'"cells": ['+
				 '[{ "klass":"given","text":"2" }, '+
				  '{ "klass":"fixed","text":"3" }, '+
				  '{ "klass":"crossedout","text":"5 6" }, '+
				  '{ "klass":"possible","text":"1 4" },'+
				  '{ "klass":"crossedout","text":"6" }, '+
				  '{ "klass":"crossedout","text":"6" }], '+
				 '[{ "klass":"given","text":"2" }, '+
				  '{ "klass":"fixed","text":"3" }, '+
				  '{ "klass":"crossedout","text":"5 6" }, '+
				  '{ "klass":"possible","text":"1 4" },'+
				  '{ "klass":"crossedout","text":"6" }, '+
				  '{ "klass":"crossedout","text":"6" }], '+
				 '[{ "klass":"given","text":"2" }, '+
				  '{ "klass":"fixed","text":"3" }, '+
				  '{ "klass":"crossedout","text":"5 6" }, '+
				  '{ "klass":"possible","text":"1 4" },'+
				  '{ "klass":"crossedout","text":"6" }, '+
				  '{ "klass":"crossedout","text":"6" }], '+
				 '[{ "klass":"given","text":"2" }, '+
				  '{ "klass":"fixed","text":"3" }, '+
				  '{ "klass":"crossedout","text":"5 6" }, '+
				  '{ "klass":"possible","text":"1 4" },'+
				  '{ "klass":"crossedout","text":"6" }, '+
				  '{ "klass":"crossedout","text":"6" }], '+
				 '[{ "klass":"given","text":"2" }, '+
				  '{ "klass":"fixed","text":"3" }, '+
				  '{ "klass":"crossedout","text":"5 6" }, '+
				  '{ "klass":"possible","text":"1 4" },'+
				  '{ "klass":"crossedout","text":"6" }, '+
				  '{ "klass":"crossedout","text":"6" }], '+
				 '[{ "klass":"given","text":"2" }, '+
				  '{ "klass":"fixed","text":"3" }, '+
				  '{ "klass":"crossedout","text":"5 6" }, '+
				  '{ "klass":"possible","text":"1 4" },'+
				  '{ "klass":"crossedout","text":"6" }, '+
				  '{ "klass":"crossedout","text":"6" } '+
				  
				']] '+
			'}';

function nextStep() {
   populateBoard(JSON.parse(example));
}

function towerCell(text) {
   var t = document.createElement("td");
   t.appendChild(document.createTextNode(text));
   t.classList.add('tower');
   return t;
}
function populateBoard(board) {
    topBottom(board.top);
	var y = 0;
	for (let line of board.cells) {
	   var row = document.createElement("tr");
	   row.appendChild(towerCell(board.left.charAt(y)));
       for (let cell of line) {
	       var td = document.createElement("td");
		   td.classList.add(cell.klass);
		   td.appendChild(document.createTextNode(cell.text));
		   row.appendChild(td);
       }	   
	   t = document.createElement("td");
	   row.appendChild(towerCell(board.right.charAt(y++)));
	   row.appendChild(t);
	   document.getElementById('board').appendChild( row );
	}
	topBottom(board.bottom);
}

function topBottom(line) {
    var htmlBoard = document.getElementById('board');
     // cells creation
    var row = document.createElement("tr");
    row.appendChild(towerCell(''));
 
    for (var i = 0; i < line.length; i++) {
      // create element <td> and text node 
      //Make text node the contents of <td> element
      // put <td> at end of the table row
      row.appendChild(towerCell(line.charAt(i)));
    }
    row.appendChild(towerCell(''));

    //row added to end of table body
    htmlBoard.appendChild(row);

}

function createForm() {
    var size = parseInt( document.getElementById('boardsize').value );
    var htmlBoard = document.getElementById('inboard');
	console.log("size is "+size);
	for (var y = 0; y < size + 2; y++) {
	   var tr = document.createElement("tr");
	   for (var x = 0; x < size + 2; x++) {
	      var td = document.createElement('td');
		  var input = document.createElement('input');
		  input.setAttribute('type','text');
		  input.setAttribute('id'  ,"x"+x+"y"+y);
		  input.setAttribute('name',"x"+x+"y"+y);
		  input.setAttribute('size',1);
		  td.appendChild(input);
		  tr.appendChild(td);
		  if (x == 0 || x > size || y == 0 || y > size) {
		      td.classList.add('tower');
		  } else {
		      td.classList.add('fixed');
		  }
	   }
       htmlBoard.appendChild(tr); 	   
	}
}
