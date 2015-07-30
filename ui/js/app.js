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
        groups:     {},
        failures:   {}
    };
    var mSelf = this;

    var parseData = function(theData) {
        if(theData.length == 0) {
            // No data to be parsed.
            return;
        }

        parseFailureData(theData);
        parseScheduleData(theData);
    }

    var parseFailureData = function(theData) {
        var i,
            aTotal,
            aFailures;

        mData.failures = [];

        // Do we have failures to deal with?
        if(theData[0] == 'Failure') {
            // Yep! :(
            aFailures = theData[1];
            for(i = 0, aTotal = aFailures.length; i < aTotal; i++) {
                mData.failures.push({
                    name: aFailures[i]._nome,
                    professor: aFailures[i]._prof
                });
            }
        }
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
        }

        console.debug(mData);
    }

    var saveData = function(theCallback) {
        $.ajax({
            url: 'api.php',
            dataType: 'json',
            method: 'post',
            data: {action: 'save', data: mData}

        }).done(function(theInfo) {
            console.debug('Data saved', theInfo);

            if(theCallback) {
                theCallback(theInfo);
            }

        }).fail(function(theJqXHR, theTextStatus, theErrorThrown) {
            console.error('Fail to save data!', theErrorThrown);
        });
    };

    var loadData = function(theCallback) {
        $.ajax({
            url: 'api.php',
            dataType: 'json',
            data: {action: 'load'}

        }).done(function(theData) {
            console.debug('Baked data loaded', theData);

            if(theData.success && theCallback) {
                if(theData.data) {
                    mData = theData.data;
                }
                theCallback();
            }

        }).fail(function(theJqXHR, theTextStatus, theErrorThrown) {
            console.error('Fail to load data!');
        });
    };

    var findExistingCourses = function(theGroup) {
        var aRet = [],
            aItens = {},
            aGroup,
            aTime,
            aDay,
            aCourse;

        for(aGroup in mData.groups) {
            // Check if we are filtering the result by group.
            if(theGroup && theGroup != aGroup) continue;

            for(aTime in mData.groups[aGroup].times) {
                for(aDay in mData.groups[aGroup].times[aTime]) {
                    aCourse = mData.groups[aGroup].times[aTime][aDay].course;
                    if(aCourse && aCourse.name != '') {
                        aItens[aCourse.name + aCourse.professor] = aCourse;
                    }
                }
            }
        }

        for(aCourse in aItens) {
            aRet.push(aItens[aCourse]);
        }

        return aRet;
    }

    var renderEverything = function(theContainerId) {
        $('#' + theContainerId).html('');

        if(mData.failures.length > 0) {
            renderFailures(theContainerId);
        }

        renderSchedule(theContainerId);
        enhanceAllElements();
    };

    var renderFailures = function(theContainerId) {
        var aText = '',
            aTotal,
            i;

        for(i = 0, aTotal = mData.failures.length; i < aTotal; i++) {
            aText += '<li>' + mData.failures[i].name + ' (' + mData.failures[i].professor + ')</li>';
        }

        $('#' + theContainerId).append(
            '<div class="row row-failures">' +
                '<div id="failures" class="col-lg-12">' +
                    '<div class="panel panel-danger">' +
                        '<div class="panel-heading"><i class="fa fa-warning"></i> Falhas de alocação automática</div>' +
                        '<div class="panel-body">' +
                            'Não foi possível alocar automaticamente na grade de horários os componentes curriculares listados abaixo. Você precisa inseri-los no horário de forma manual.' +
                            '<ul>' +
                                aText +
                            '</ul>' +
                        '</div>' +
                    '</div>' +
                '</div>' +
            '</div>'
        );
    };

    var renderSchedule = function(theContainerId) {
        var aGroup;

        for(aGroup in mData.groups) {
            $('#' + theContainerId).append(
                '<div class="row schedule-row">' +
                    '<div id="group-'+ aGroup +'" class="col-lg-12"></div>' +
                '</div>' +

                '<div class="row">' +
                    '<div class="row-ui-professor-manager row-ui-professor-manager-' + aGroup +' col-lg-12">' +
                        '<div class="panel panel-default info">' +
                          '<i class="fa fa-user-plus fa-3x"></i>' +
                          '<div class="panel-body">' +
                          '</div>' +
                        '</div>' +
                    '</div>' +
                '</div>'+

                '<div class="row schedule-caption-row">' +
                    '<div id="caption-'+ aGroup +'" class="col-lg-12"></div>' +
                '</div>'
            );

            renderGroupSchedule(aGroup, {title: 'Fase ' + aGroup});
            renderGroupCaption(aGroup);
        }
    }

    var renderGroupCaption = function(theGroupId) {
        var aId,
            i,
            aRet = [],
            aTotal,
            aCourses = findExistingCourses(theGroupId);

        for(i = 0, aTotal = aCourses.length; i < aTotal; i++) {
            aRet.push('<li>' + aCourses[i].name + ' (' + aCourses[i].professor + ') </li>');
        }

        $('#caption-' + theGroupId).html(aRet.join(''));
    }

    var renderGroupSchedule = function(theGroupId, theLabels) {
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

                aContent += '<td class="clickable" data-course="' + aCourseName + '" data-group="' + theGroupId + '" data-day="' + aWeekDay + '" + data-time="' + aTime + '">' + renderCellContent(aInfo) + '</td>';
            }
            aContent += '</tr>';
        }

        aTable +=
            '<h3>' + (theLabels.title || 'No title') + '</h3>' +
            '<table class="table table-bordered schedule-table">' +
                '<tr class="success">' +
                    '<th>Horário</th>' +
                    '<th>Segunda-feira</th>' +
                    '<th>Terça-feira</th>' +
                    '<th>Quarta-feira</th>' +
                    '<th>Quinta-feira</th>' +
                    '<th>Sexta-feira</th>' +
                '</tr>' +
                aContent +
            '</table>';

        $('#group-'+ theGroupId +'').html(aTable);
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

        aInfoA.locked   = false;
        aInfoB.locked   = false;

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

    var handleCellHoverIn = function() {
        $(this).find('.cell-buttons').show();
    };

    var handleCellHoverOut = function() {
        $(this).find('.cell-buttons').hide();
    };

    var insertNewProfessorFormInto = function(theContainer, theGroupId) {
        var aForm,
            aFormId = 'formProfessor' + theGroupId;

        aForm = '' +
            '<form class="form-inline" id="' + aFormId + '">' +
              '<div class="form-group">' +
                '<label for="formProfessorCourse">Componente curricular</label> ' +
                '<input type="text" class="form-control" name="course" id="formProfessorCourse" placeholder="Ex. Algoritmos">' +
              '</div>' +
              '<div class="form-group">' +
                '<label for="formProfessorProfessor">Professor</label> ' +
                '<input type="text" class="form-control" name="professor" id="formProfessorProfessor" placeholder="Ex. Fernando">' +
              '</div>' +
              ' <button type="submit" class="btn btn-primary">Adicionar</button>' +
            '</form>';

        theContainer.html(aForm);

        return aFormId;
    }

    var generateProfessorsOptionList = function(theSelectedCourseName) {
        var aRet = '',
            aCourses,
            i,
            aTotal,
            aSelected;

        aCourses = findExistingCourses();

        for(i = 0, aTotal = aCourses.length; i < aTotal; i++) {
            aSelected = theSelectedCourseName == aCourses[i].name ? 'selected="selected"' : '';
            aRet += '<option value="' + aCourses[i].professor + '###' + aCourses[i].name + '" ' + aSelected + '>' + aCourses[i].name + ' (' + aCourses[i].professor + ')</option>';
        }

        return '<option value="###">Selecione...</option><option value=""></option>' + aRet;
    }

    var insertSelectProfessorFormInto = function(theContainer, theGroupId, theSelectedCourseName) {
        var aContent,
            aForm,
            aValues,
            aFormId = 'formProfessor' + theGroupId;

        aContent = '' +
            '<form class="form-inline" id="' + aFormId + '">' +
              '<div class="form-group">' +
                '<input type="hidden" name="course" />' +
                '<input type="hidden" name="professor" />' +
                '<select name="professorSelection" id="professorSelection' + theGroupId + '" class="form-control">' +
                    generateProfessorsOptionList(theSelectedCourseName) +
                '</select>' +
              '</div>' +
            '</form>';

        theContainer.html(aContent);

        // When the user selects something on the dropdown,
        // we update the internal form and send the data away.
        $('#professorSelection' + theGroupId).on('change blur focusout', function() {
            aForm   = document.getElementById(aFormId);
            aValues = $(this).val().split('###');

            aForm.elements.course.value     = aValues[1] || '';
            aForm.elements.professor.value  = aValues[0] || '';

            $('#' + aFormId).submit();
        });

        return aFormId;
    }

    var updateDataEntry = function(theGroup, theTime, theDay, theCourse, theProfessor) {
        var aInfo;

        if(!mData.groups[theGroup].times[theTime][theDay]) {
            mData.groups[theGroup].times[theTime][theDay] = {
                day: 0,
                time: 0,
                locked: false,
                course: {
                    name: '',
                    professor: ''
                }
            };
        }

        aInfo = mData.groups[theGroup].times[theTime][theDay];

        aInfo.day   = theDay;
        aInfo.time  = theTime;

        aInfo.course.name       = theCourse;
        aInfo.course.professor  = theProfessor;

        console.debug('Data has changed.', mData);
    };

    var handleLockerClick = function() {
        var aElement    = $(this),
            aCell       = aElement.parent().parent(), // TODO: remove all those parent() stuff
            aCourseInfo;

        aCourseInfo = getCourseInfoByCell(aCell);

        if(aCourseInfo) {
            aCourseInfo.locked = aCourseInfo.locked === false || aCourseInfo.locked == "false" ? true : false;
        }

        updateCellContent(aCell);
    };

    var handleNewProfessorClick = function() {
        var aElement    = $(this),
            aUseSelect  = aElement.hasClass('select-professor'),
            aCell       = aElement.parent().parent(), // TODO: remove all those parent() stuff
            aGroup      = aCell.data('group'),
            aContainer  = $('div.row-ui-professor-manager-' + aGroup),
            aForm,
            aFormId;

        // For the sake of usability, siable all clickable behavior
        // for this cell. It will be enabled back when the user is
        // done editing the cell content.
        aCell.off().removeClass('selected');

        // Check what type of professor addition it is. It can be a "dropdown" selecting
        // one where the user chooses the professor name from a list. Or it can be the
        // addition of a brand new professor, made in a separate UI.
        if(aUseSelect) {
            // Select from dropdown.
            aFormId = insertSelectProfessorFormInto(aCell, aGroup, aCell.data('course'));

        } else {
            // Use the separate UI to allow the user to input the new professor info.
            aFormId = insertNewProfessorFormInto(aContainer.find('.panel-body'), aGroup);
            aContainer.hide().slideDown();
        }

        // Handle the submition of the UI form fired when
        // the user is done editing everything.
        $('#' + aFormId).submit(function() {
            aForm = document.getElementById(aFormId);

            // Update the internal schedule database
            updateDataEntry(aGroup,
                            aCell.data('time'),
                            aCell.data('day'),
                            aForm.elements.course.value,
                            aForm.elements.professor.value);

            // Update and refresh the selected cell with new course info
            aCell.data('course', aForm.elements.course.value);
            updateCellContent(aCell);

            // Update group caption
            renderGroupCaption(aGroup);

            // Hide the editing UI, if needed
            if(!aUseSelect) {
                aContainer.slideUp();
            }

            // Restore cell clickable behavior
            enhanceAllElements();

            return false;
        });
    };

    var handleRemoveProfessorClick = function() {
        var aCell = $(this).parent().parent(); // TODO: remove all those parent() stuff

        if(confirm('Remover mesmo?')) {
            // Update the internal schedule database
            updateDataEntry(aCell.data('group'), aCell.data('time'), aCell.data('day'), '', '');

            // Update and refresh the selected cell with empty course info
            aCell.data('course', '');
            updateCellContent(aCell);

            // Update group caption
            renderGroupCaption(aCell.data('group'));

            // Restore clickable behavior
            enhanceAllElements();
        }
    };

    var renderCellContent = function(theCourseData) {
        var aRet = '',
            aIsLocked = false,
            aCourseName;

        aRet += '<a href="javascript:void(0)" class="select-professor" title="Selecionar professor da lista de cadastrado"><i class="fa fa-toggle-down"></i></a>';

        if(theCourseData && theCourseData.course) {
            aCourseName = theCourseData.course.name || '';
            aRet += '<a href="javascript:void(0)" class="remove-professor" title="Remover professor"><i class="fa fa-trash"></i></a>';

            if(theCourseData.locked == "true" || theCourseData.locked === true) {
                aIsLocked = true;
                aRet += '<a href="javascript:void(0)" class="remove-lock" title="Remover trancamento dessa posição (faz esse professor ficar móvel durante a alocação automática)."><i class="fa fa-unlock"></i></a>';

            } else {
                aRet += '<a href="javascript:void(0)" class="add-lock" title="Trancar professor nessa posição (esse professor não sofrerá movimentações durante a alocação automática)."><i class="fa fa-lock"></i></a>';
            }
        } else {
            aRet += '<a href="javascript:void(0)" class="add-professor" title="Adicionar um novo professor"><i class="fa fa-plus-circle"></i></a>';
        }

        if(aIsLocked) {
            aCourseName = '<span class="locked-name">' + aCourseName + '</span>';
        }

        return aCourseName + '<span class="cell-buttons pull-right">' + aRet + '</span>' + (aIsLocked ? '<i class="fa fa-lock badge-locked" title="Esse professor ficará fixado a essa posição durante a alocação automática."></i>' : '');
    };

    var updateCellContent = function(theObject) {
        theObject.fadeOut('fast', function() {
            $(this).html(renderCellContent(getCourseInfoByCell(theObject))).fadeIn('fast');
            enhanceAllElements();
        });
    };

    var enhanceAllElements = function() {
        $('#main td.clickable').each(function(theIndex, theElement) {
            $(theElement).off();
            $(theElement).click(handleCellClick);
            $(theElement).hover(handleCellHoverIn, handleCellHoverOut);
        });

        $('#main a.add-professor, #main a.select-professor').each(function(theIndex, theElement) {
            $(theElement).off();
            $(theElement).click(handleNewProfessorClick);
        });

        $('#main a.add-lock, #main a.remove-lock').each(function(theIndex, theElement) {
            $(theElement).off();
            $(theElement).click(handleLockerClick);
        });

        $('#main a.remove-professor').each(function(theIndex, theElement) {
            $(theElement).off();
            $(theElement).click(handleRemoveProfessorClick);
        });
    };

    var handleNavbarSave = function() {
        var aButton = $('#navbar a.save');

        // Show some nice UI loading anim
        aButton.html('<i class="fa fa-circle-o-notch fa-spin"></i>');

        // Send data to server
        saveData(function(theInfo) {
            if(theInfo.success) {
                aButton.html('<span id="save-caption">Ok, salvo!</span> ');
            } else {
                aButton.html('<span id="save-caption">Oops, algo deu errado!</span> ');
            }
            aButton.append('<i class="fa fa-save"></i>');

            $('#save-caption').fadeIn().delay(2000).fadeOut();
        });
    };

    var handleNavbarPrint = function() {
        // TODO: implement the prinf feature.
    };

    var handleNavbarMagic = function() {
        $('#main').html('<div class="alert alert-warning magic-loading-panel" role="alert"><strong><i class="fa fa-rocket fa-spin fa-2x"></i> Trabalhando!</strong> Um novo horário está sendo gerado, por favor, aguarde. Isso pode levar vários minutos.</div>').fadeIn();

        $.ajax({
            url: 'api.php',
            dataType: 'json',
            data: {action: 'magic', config: 'config'}

        }).done(function(theData) {
            console.debug('Magic data loaded', theData);

            if(theData.success) {
                parseData(theData.data);
                renderEverything('main');

            } else {
                console.error(theData);
            }
        }).fail(function(theJqXHR, theTextStatus, theErrorThrown) {
            console.error('Fail to load magic data!');
        });

    };

    var buildNavbar = function() {
        $('#navbar a.save').click(handleNavbarSave);
        $('#navbar a.print').click(handleNavbarPrint);
        $('#navbar a.magic').click(handleNavbarMagic);
    };

    this.init = function() {
        buildNavbar();

        $('#main').html('<div class="alert alert-warning loading-panel" role="alert"><i class="fa fa-circle-o-notch fa-spin"></i> <strong>Carregando dados</strong>. Por favor, aguarde.</div>').fadeIn();

        loadData(function() {
            renderEverything('main');
        });
    }
};


$(function() {
    HORARIO.init();
});
