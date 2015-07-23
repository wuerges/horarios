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
        courses:    {},
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
                course:     {name: aEntry[2]._nome, professor: aEntry[2]._prof},
                locked:     false
            };

            // Colect information related to professors
            mData.professors[aGroup] = mData.professors[aGroup] || {};
            mData.professors[aGroup][aEntry[2]._prof] = mData.professors[aGroup][aEntry[2]._prof] || {name: '', courses: {}};
            mData.professors[aGroup][aEntry[2]._prof].name = aEntry[2]._prof;
            mData.professors[aGroup][aEntry[2]._prof].courses[aEntry[2]._nome] = true;

            // Colect information related to courses
            mData.courses[aEntry[2]._nome] = aEntry[2]._nome;
        }

        console.debug(mData);
    }

    var loadData = function(theCallback) {
        $.ajax({
            url: '../inputs/teste3_json.out', // TODO: use correct backend URL here
            dataType: 'json'
        }).done(function(theData) {
            parseData(theData);
            console.debug(mData);

            if(theCallback) {
                theCallback();
            }

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
                        renderGroup(aGroup, {title: 'Fase ' + aGroup}) +
                    '</div>' +
                '</div>' +
                '<div class="row schedule-caption-row">' +
                    '<div id="caption-'+ aGroup +'" class="col-lg-10">' +
                        renderGroupCaption(aGroup) +
                    '</div>' +
                '</div>'
            );
        }
    }

    var renderGroupCaption = function(theGroupId) {
        var aName,
            aId,
            aCourse,
            aRet = [],
            aData = mData.professors[theGroupId];

        for(aName in aData) {
            for(aCourse in aData[aName].courses) {
                aRet.push('<li>' + aCourse + ' (' + aName + ') </li>');
            }
        }

        aRet.push('<li><a href="javascript:void(0)" data-group="' + theGroupId + '" class="add-professor"><i class="fa fa-user-plus"></i> Adicionar</a></li>');

        return aRet.join('');
    }

    var renderGroup = function(theGroupId, theLabels) {
        var aContent = '',
            aTable = '',
            aTime,
            aDays,
            aInfo,
            aCourseName,
            aWeekDay,
            aData = mData.groups[theGroupId];

        theLabels = theLabels || {};

        for(aTime in aData.times) {
            aDays = aData.times[aTime];
            aContent += '<tr>';
            aContent += '<td>' + aTime + '</td>';

            for(aWeekDay = 1; aWeekDay <= 5; aWeekDay++) {
                aInfo       = aDays[aWeekDay];
                aCourseName = aInfo ? aInfo.course.name : '';

                aContent += '<td class="clickable" data-course="' + aCourseName + '" data-group="' + theGroupId + '" data-day="' + aWeekDay + '" + data-time="' + aTime + '">' + aCourseName + '</td>';
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

    var getCourseInfoByCell = function(theCell) {
        return getCourseInfoByMeta(theCell.data('group'), theCell.data('time'), theCell.data('day'));
    };

    var getCourseInfoByMeta = function(theGroup, theTime, theDay) {
        return mData.groups[theGroup].times[theTime][theDay];
    };

    var swapCoursesByCell = function(theCellA, theCellB) {
        var aInfoA,
            aInfoB,
            aCourse;

        aInfoA = getCourseInfoByCell(theCellA);
        aInfoB = getCourseInfoByCell(theCellB);

        aCourse         = aInfoA.course;
        aInfoA.course   = aInfoB.course;
        aInfoB.course   = aCourse;

        theCellA.data('course', aInfoA.course.name);
        theCellB.data('course', aInfoB.course.name);

        updateCellContent(theCellB);
        updateCellContent(theCellA);
    };

    var handleCellClick = function() {
        var aCurrent = $(this),
            aOther,
            aCourse;

        if(aCurrent.hasClass('selected')) {
            aCurrent.removeClass('selected');

        } else {
            aOther = $('td.selected.clickable').first()[0];

            // Is there anything already selected?
            if(aOther) {
                // Yep! Let's swap the two selected items
                aOther  = $(aOther);
                swapCoursesByCell(aOther, aCurrent);

                aOther.removeClass('selected');

            } else {
                aCurrent.addClass('selected');
            }
        }
    };

    var generateNewProfessorForm = function(theGroup) {
        var aRet =
            '<form class="form-inline" id="formProfessor' + theGroup + '">' +
              '<div class="form-group">' +
                '<input type="text" class="form-control" name="course" placeholder="Componente currucular">' +
              '</div>' +
              '<div class="form-group">' +
                '<input type="text" class="form-control" name="professor" placeholder="Professor">' +
              '</div>' +
              '<button type="submit" class="btn btn-default"><i class="fa fa-plus"></i> Adicionar</button>' +
            '</form>';

        return aRet;
    }

    var handleNewProfessorClick = function() {
        var aElement    = $(this),
            aGroup      = aElement.data('group'),
            aContainer  = aElement.parent(),
            aForm;

        aContainer.html(generateNewProfessorForm(aGroup));

        $('#formProfessor' + aGroup).submit(function() {
            // TODO: implement the remaining parts
            aForm = document.getElementById('formProfessor' + aGroup);
            aContainer.html(aForm.elements.course.value + ' (' + aForm.elements.professor.value +')');

            return false;
        });
    };

    var renderCellContent = function(theInfo) {
    };

    var updateCellContent = function(theObject) {
        theObject.fadeOut('fast', function() {
            $(this).html(theObject.data('course')).fadeIn('fast');
        });
    };

    var enhance = function() {
        $('#main td.clickable').each(function(theIndex, theElement) {
            $(theElement).click(handleCellClick);
        });

        $('#main a.add-professor').each(function(theIndex, theElement) {
            $(theElement).click(handleNewProfessorClick);
        });
    };

    this.init = function() {
        loadData(function() {
            renderSchedule('main');
            enhance();
        });
    }
};


$(function() {
    HORARIO.init();
});
