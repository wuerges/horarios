/*
	The MIT License (MIT)

	Copyright (c) 2015 Fernando Bevilacqua

	Permission is hereby granted, free of charge, to any person obtaining a copy of
	this software and associated documentation files (the "Software"), to deal in
	the Software without restriction, including without limitation the rights to
	use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
	the Software, and to permit persons to whom the Software is furnished to do so,
	subject to the following conditions:

	The above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
	FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
	COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
	IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
	CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

var HORARIO = new function() {

    this.renderGroup = function(theContainerId, theData, theLabels) {
        var aContent = '',
            aTable = '',
            aTime,
            aWeekDay;

        theLabels = theLabels || {};

        for(aTime = 0; aTime < 5; aTime++) {
            aContent += '<tr>';
            for(aWeekDay = 0; aWeekDay < 6; aWeekDay++) {
                aContent += '<td>' + aWeekDay + '</td>';
            }
            aContent += '</tr>';
        }

        aTable +=
            '<h3>' + (theLabels.title || 'No title') + '</h3>' +
            '<table class="table table-bordered">' +
                '<tr>' +
                    '<th>Horário</th>' +
                    '<th>Segunda-feira</th>' +
                    '<th>Terça-feira</th>' +
                    '<th>Quarta-feira</th>' +
                    '<th>Quinta-feira</th>' +
                    '<th>Sexta-feira</th>' +
                '</tr>' +
                aContent +
            '</table>';

        $('#' + theContainerId).html(aTable);
    };
};


$(function() {
    HORARIO.renderGroup('group-2', null, {title: '2a. Fase (Matutino)'});
    HORARIO.renderGroup('group-3', null, {title: '4a. Fase (Matutino)'});
    console.log('hey!');
});
