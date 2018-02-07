JsOsaDAS1.001.00bplist00—Vscript_z// DRAFT 0.03

// Import Reminders from specified list
// adding to start or end of TaskPaper 3 file,
// or to start of a named project

// See options at end of script

(function (dctOptions) {
    'use strict';

    // priorityName :: Int -> String
    function priorityName(intLevel) {
        if (intLevel > 6) return 'low';
        else if (intLevel < 4) return 'hi';
        else return 'med';
    }

    // Date -> String
    function fmtTP(dte) {
        var s = dte.toISOString(),
            d = s.substring(0, 10);

        return dte.getMinutes() ? d + ' ' + s.substring(11, 16) : d;
    }

    // fileExists :: String -> Bool
    function fileExists(strPath) {
        var fm = $.NSFileManager.defaultManager,
            error = $();
        fm.attributesOfItemAtPathError(
            ObjC.unwrap($(strPath)
                .stringByExpandingTildeInPath),
            error
        );
        return error.code === undefined;
    }

    // readFile :: FilePath -> IO String
    function readFile(strPath) {
	var error = $(),
            str = ObjC.unwrap(
                $.NSString.stringWithContentsOfFileEncodingError($(strPath)
                    .stringByStandardizingPath, $.NSUTF8StringEncoding, error)
            );
			return str;
//        return ObjC.unwrap($.NSString.stringWithContentsOfFile(strPath));
    }

    //writeFile :: FilePath -> String -> IO ()
    function writeFile(strPath, strText) {
        $.NSString.alloc.initWithUTF8String(strText)
            .writeToFileAtomicallyEncodingError(
                strPath, false,
                $.NSUTF8StringEncoding, null
            );
    }
	

    // concatMap :: (a -> [b]) -> [a] -> [b]
    function concatMap(f, xs) {
        return [].concat.apply([], xs.map(f));
    }

	//var app = Application.currentApplication()
	//app.includeStandardAdditions = true

    var Reminders = Application('Reminders'),
        lstMatch = Reminders.lists.whose({
            _match: [ObjectSpecifier()
                .name, dctOptions.RemindersListName]
        }),
        list = lstMatch.length > 0 ? lstMatch[0] : undefined;
	
    if (list) {
        var strPath = $(dctOptions.TaskPaper3FilePath)
            .stringByExpandingTildeInPath.js;

        if (fileExists(strPath)) {
            var blnAtStart = dctOptions.atStart,
                oReminders = list.reminders,
                lstRem = oReminders.whose({
                    _match: [ObjectSpecifier()
                .completed, false]
                })(),

                lstTodo = concatMap(function (r) {
                    try {
                        var strBody = r.body(),
                            dteDue = undefined, // r.dueDate(),
                            lngHeat = r.priority();
                    } catch (e) {
                        return [];
                    }
                        var tmp = strBody ? strBody.split(
                                /[\n\r]+/)
                            .map(function (para) {
                                return '\t' + para;
                            }) : undefined
						tmp = tmp ? tmp[tmp.length - 1] : []
                        var lines = [[
                            '- ',
                            r.name(),
                            dteDue ? ' @due(' + fmtTP(dteDue) + ')' : '',
                            lngHeat ? ' @priority(' + priorityName(lngHeat) + ')' : ''
                        ].join('')].concat(tmp)

                    return [{
                        id: r.id(),
						lines: lines
                    }];
                }, lstRem);

            if (lstTodo.length > 0) {

                var strProject = dctOptions.projectName,
                    strHeader = strProject ? strProject : (
                        dctOptions.useListName ? list.name() : ''
                    ),
                    strLabel = strHeader + ':';
					
                if (strHeader && strHeader.length) {
                    // ADD AFTER HEADER, CREATING IF NOT FOUND
					
                    var rgxProj = new RegExp(
                            '^(\\s*)' + strHeader + ':.*$',
                            'mi'
                        ),
                        xs = readFile(strPath)
                        .split(/[\n\r]/),
                        i = xs.length,
                        dctProj,
                        x, m;
						
                    while (i--) {
                        if (xs[i].indexOf(strLabel) !== -1) {
                            m = xs[i].match(rgxProj);
                            if (m) {
                                dctProj = {
                                    indent: m[1].replace(
                                            /    /, '\t'
                                        )
                                        .length,
                                    pre: xs.slice(0, i + 1),
                                    post: xs.slice(i + 1)
                                }
                                break;
                            };
                        }
                    }

                    if (!dctProj) {
                        dctProj = {
                            indent: 0,
                            pre: (blnAtStart ? [] : xs)
                                .concat(strLabel),
                            post: blnAtStart ? xs : []
                        }
                    }

                    var strIndent = Array(dctProj.indent + 2)
                        .join('\t'),
                        strNew = dctProj.pre.join('\n') + '\n' +
                        lstTodo.map(function (x) {
                            return x.lines.map(
                                function (para) {
                                    return strIndent + para;
                                })
                            .join('\n');
                        })
                        .join('\n') + '\n' +
                        dctProj.post.join('\n');

                    writeFile(
                        strPath,
                        strNew
                    );


                    // SIMPLY ADD TO START OR END OF FILE
                } else {

                    var lstUpdate = [
                        lstTodo.map(function (x) {
                            return x.lines.join('\n');
                        })
                        .join('\n'),
                        '\n\n',
                        readFile(strPath)
                    ];

                    writeFile(
                        strPath,
                        (blnAtStart ? lstUpdate :
                            lstUpdate
                            .reverse())
                        .join('')
                    )
                }

                // IF REQUIRED, CHECK THE IMPORT AND DELETE FROM REMINDERS.APP

                if (dctOptions.deleteReminderAfterImport) {

                    var strUpdated = readFile(strPath);

                    return concatMap(function (dct) {
                        var strName = dct.lines[0];
                        if (strUpdated.indexOf(strName) !== -1) {
                            Reminders.delete(
                                oReminders.byId(dct.id)
                            );
                            return [strName];
                        } else {
                            return [];
                        }
                    }, lstTodo);
                }

            } // lstTodo.length > 0

        } else return 'File not found: ' + dctOptions.TaskPaper3FilePath;

    } else return 'No list named "' + dctOptions.RemindersListName +
        '" found in Reminders';

})({
    RemindersListName: 'Reminders',
    TaskPaper3FilePath: '/Users/ebowman/Dropbox/Apps/Editorial/TaskPaper/things.taskpaper',

    projectName: 'Inbox', // Project to find or create (to hold imports)
    useListName: true, // Use Reminders list name for above
    atStart: true, // start or end of file if using neither project nor list name ?

    deleteReminderAfterImport: true
});




                              êjscr  ˙ﬁﬁ≠