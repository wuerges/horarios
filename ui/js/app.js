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
                    'times': {}
                };
            }

            mData.groups[aGroup]['times'][aEntry[0]._hora] = mData.groups[aGroup]['times'][aEntry[0]._hora] || {};

            mData.groups[aGroup]['times'][aEntry[0]._hora][aEntry[0]._dia] = {
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
            renderSchedule('main');

        }).fail(function(theJqXHR, theTextStatus, theErrorThrown) {
            console.error('Fail!');
        });
    };

    var renderSchedule = function(theContainerId) {
        var aGroup;

        for(aGroup in mData.groups) {
            $('#' + theContainerId).append(
                '<div class="row schedule-row">' +
                    '<div id="group-'+ aGroup +'" class="col-lg-10">' +
                        renderGroup(mData.groups[aGroup], {title: 'Fase ' + aGroup}) +
                    '</div>' +
                '</div>' +
                '<div class="row schedule-caption-row">' +
                    '<div id="caption-'+ aGroup +'" class="col-lg-10">' +
                        renderGroupCaption(mData.professors[aGroup]) +
                    '</div>' +
                '</div>'
            );
        }
    }

    var renderGroupCaption = function(theProfessors) {
        var aName,
            aId,
            aCourse,
            aRet = [];

        for(aName in theProfessors) {
            for(aCourse in theProfessors[aName].courses) {
                aRet.push('<li>' + aCourse + ' (' + aName + ') </li>');
            }
        }

        return aRet.join('');
    }

    var renderGroup = function(theData, theLabels) {
        var aContent = '',
            aTable = '',
            aTime,
            aDays,
            aInfo,
            aWeekDay;

        theLabels = theLabels || {};

        for(aTime in theData.times) {
            aDays = theData.times[aTime];
            aContent += '<tr>';
            aContent += '<td>' + aTime + '</td>';

            for(aWeekDay = 1; aWeekDay <= 5; aWeekDay++) {
                aInfo = aDays[aWeekDay];

                if(aInfo) {
                    aContent += '<td>' + aInfo.course + '</td>';
                } else {
                    aContent += '<td></td>';
                }
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

        return aTable;
    };

    this.init = function() {
        loadData();
    }
};


$(function() {
    HORARIO.init();
});
