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
    var mData = {
        professors: {},
        groups:     {},
        failures:   {}
    };
    var mSelf = this;

    var parseData = function(theData) {
        parseFailureData(theData);
        parseScheduleData(theData);
    }

    var parseFailureData = function(theData) {
        mData.failures = theData[0] == 'Failure' ? theData[1] : []
    };

    var parseScheduleData = function(theData) {
        var aFailures,
            aSchedules,
            aEntry,
            i,
            j,
            aGroup;

        aSchedules = theData[2];

        for(i = 0; i < aSchedules.length; i++) {
            aEntry = aSchedules[i];
            aGroup = aEntry[1];

            if(mData.groups[aGroup] == null) {
                mData.groups[aGroup] = {
                    'days': {}
                };
            }

            mData.groups[aGroup]['days'][aEntry[0]._dia] = mData.groups[aGroup]['days'][aEntry[0]._dia] || {};

            mData.groups[aGroup]['days'][aEntry[0]._dia][aEntry[0]._hora] = {
                day:        aEntry[0]._dia,
                time:       aEntry[0]._hora,
                professor:  aEntry[2]._prof,
                course:     aEntry[2]._nome
            };

            // Colect information related to professors
            mData.professors[aGroup] = mData.professors[aGroup] || {};
            mData.professors[aGroup][aEntry[2]._prof] = mData.professors[aGroup][aEntry[2]._prof] || {name: '', courses: {}};
            mData.professors[aGroup][aEntry[2]._prof].name = aEntry[2]._prof;
            mData.professors[aGroup][aEntry[2]._prof].courses[aEntry[2]._nome] = true;
        }
    }

    var loadData = function() {
        $.ajax({
            url: '../inputs/teste3_json.out', // TODO: use correct backend URL here
            dataType: 'json'
        }).done(function(theData) {
            parseData(theData);

        }).fail(function(theJqXHR, theTextStatus, theErrorThrown) {
            console.error('Fail!');
        });
    };

    var renderGroup = function(theContainerId, theData, theLabels) {
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

    this.init = function() {
        loadData();
    }
};


$(function() {
    HORARIO.init();
    console.log('hey!');
});
