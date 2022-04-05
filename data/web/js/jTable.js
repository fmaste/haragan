// jTable
/******************************************************************************/

this.jTable = (function ( window, document, $, undefined ) {
        // Private static properties/methods here.
        // The returned jQuery like object.
        var Library = function ( tableSelector ) {
                // With the provided params create a new instance of the object.
                var instance = new init( tableSelector );
                // For every Library(params) a new instance is returned.
                return instance;
        };
        // Public static properties/methods here.
        Library.version = "0.0.1";
        // The constructor.
        var init = function ( tableSelector ) {
                // Fundamental variables.
                //------------------------------------------------------------------------
                var _that = this;
                var _table = $( tableSelector );
                var _loadUUID = 0; // Don't fill the table with a new request in progress.
                var _columnsArray = []; // Columns IDs ordered by creation time.
                var _columnsMap = {}; // Columns settings by ID.
                var _rowsData = {}; // Store rows data here instead of HTML 5 data attr.
                var _filters = {}; // Filter functions by name.
                // Properties variables.
                //------------------------------------------------------------------------
                var _introspectionTableName = undefined; // The one to load from server.
                var _headerShown = false; // Is shown by the user ?
                var _footerShown = false; // Is shown by the user ?
                var _selectorSettings = undefined;
                var _selectorAccum = {}; // Selector memory.
                var _searchString = undefined;
                var _limitNumberBeforeLoad = undefined;
                var _limitNumber = undefined;
                var _offsetNumberBeforeLoad = undefined;
                var _offsetNumber = undefined;
                var _recordsTotal = undefined;
                var _recordsFiltered = undefined;
                var _loadDeferred = $.Deferred();
                var _onRowAddedCallbacks = $.Callbacks();
                var _onRowRemovedCallbacks = $.Callbacks();
                var _onRowClickCallbacks = $.Callbacks();
                var _onRowRightClickCallbacks = $.Callbacks();
                var _onSelectorChangedCallbacks = $.Callbacks();
                var _onSearchChangedCallbacks = $.Callbacks();
                var _onLimitChangedCallbacks = $.Callbacks();
                var _onOffsetChangedCallbacks = $.Callbacks();
                var _onRecordsTotalChangedCallbacks = $.Callbacks();
                var _onRecordsFilteredChangedCallbacks = $.Callbacks();
                var _onLoadCallbacks = $.Callbacks();
                var _onLoadedCallbacks = $.Callbacks();
                var _onReloadCallbacks = $.Callbacks();
                var _onReloadedCallbacks = $.Callbacks();
                var _onClearCallbacks = $.Callbacks();
                var _onDestroyCallbacks = $.Callbacks();
                // Clean the table.
                //----------------------------------------------------------------------
                _table.empty();
                // Create the table structure.
                var _thead = $( "<thead></thead>" ).appendTo( _table );
                var _theadTr = $( "<tr></tr>" ).appendTo( _thead );
                var _tBody = $( "<tbody></tbody>" ).appendTo( _table );
                var _tfoot = $( "<tfoot></tfoot>" ).appendTo( _table );
                var _tfootTr = $( "<tr></tr>" ).appendTo( _tfoot );
                // Methods.
                //----------------------------------------------------------------------
                this.column = function ( param1, param2 ) {
                        // Multiple columns, an array of {id: , name:, etc ...}.
                        if ( $.isArray( param1 ) ) {
                                var columnsArray = param1;
                                for ( var i = 0; i < columnsArray.length; i++ ) {
                                        var columnSettings = columnsArray[ i ];
                                        that.column( columnSettings );
                                }
                        // One column, an object like {id: , name:, etc ...}.
                        } else if ( $.isPlainObject( param1 ) ) {
                                var columnSettings = param1;
                                var columnId = columnSettings.id;
                                if ( typeof columnId === "string" && columnId !== "" ) {
                                        return that.column( columnId, columnSettings );
                                }
                        // Do something with one column.
                        } else if ( typeof param1 === "string" && param1 !== "" ) {
                                var columnId = param1;
                                var columnSettingsOld = _columnsMap[ columnId ];
                                // Add or replace one column.
                                if ( $.isPlainObject( param2 ) ) {
                                        var columnSettingsNew = param2;
                                        // New column.
                                        if ( columnSettingsOld === undefined ) {
                                                columnSettingsNew.columnId = columnId;
                                                columnSettingsNew.columnOrder = _columnsArray.length;
                                                _columnsMap[ columnId ] = columnSettingsNew;
                                                _columnsArray.push( columnId );
                                        // Replace column.
                                        } else {
                                                columnSettingsNew.columnId = columnId;
                                                columnSettingsNew.columnOrder = columnSettingsOld.columnOrder;
                                                _columnsMap[ columnId ] = columnSettingsNew;
                                                _columnsArray[ columnSettingsOld.columnOrder ] = columnId;
                                        }
                                        // Recreate header ?
                                        if ( _headerShown === true ) {
                                                _that.header();
                                        }
                                        // Recreate footer ?
                                        if ( _footerShown === true ) {
                                                _that.footer();
                                        }
                                        // TODO: Recreate cells.
                                        _tBody.find( 'tr' ).each( function () {

                                        });
                                // Do something else with an existing column.
                                } else if ( columnSettingsOld !== undefined ) {
                                        var columnOrder = columnSettingsOld.columnOrder;
                                        // Get a jQuery object with the cell for this row id.
                                        if ( typeof param2 === "string" ) {
                                                var rowId = param2;
                                                var rows = _tBody.find( 'tr' ).filter( function () {
                                                        return Number( $( this ).data( "rowId" ) ) === rowId;
                                                });
                                                var cell = rows.first().find( 'td' ).get( columnOrder );
                                                return $( cell );

                                        //}
                                        // TODO: Rename column.
                                        //if ( typeof param2 === "string" ) {
                                        //        columnSettingsOld.name = param2;
                                        //        return _that.column( columnId, columnSettingsOld );

                                        // TODO: Remove column.
                                        //} else if ( param2 === null ) {

                                        // Get a jQuery object with all the cells on this column.
                                        } else if ( param2 === undefined ) {
                                                var ans = $();
                                                _tBody.find( 'tr' ).each( function () {
                                                        var row = $( this );
                                                        var columnCell = row.find( 'td' ).get( columnOrder );
                                                        ans = ans.add( $( columnCell ) );
                                                });
                                                return ans;
                                        }
                                }
                        // Do the same as all the above cases but with the position.
                        } else if ( typeof param1 === "number" ) {
                                var columnOrder = param1;
                                var columnId = _columnsArray[ columnOrder ];
                                if ( columnId !== undefined ) {
                                        return _that.column( columnId, param2 );
                                } else {
                                        return null;
                                }
                        }
                        return _that;
                };
                this.header = function ( maybe ) {
                        // Show or redraw it.
                        if ( maybe === undefined || maybe === true ) {
                                // Mark as shown.
                                _headerShown = true;
                                // Clear the header.
                                _theadTr.empty();
                                for ( var i = 0; i < _columnsArray.length; i++ ) {
                                        var columnId = _columnsArray[ i ];
                                        var columnSettings = _columnsMap[ columnId ];
                                        var theadTh = $( "<th></th>" ).appendTo( _theadTr );
                                        theadTh.data( "columnId", columnId );
                                        theadTh.text( columnSettings.name );
                                        if ( $.isPlainObject( columnSettings.hidden ) ) {
                                                if ( columnSettings.hidden.xs ) {
                                                        theadTh.addClass( "hidden-xs" );
                                                }
                                                if ( columnSettings.hidden.sm ) {
                                                        theadTh.addClass( "hidden-sm" );
                                                }
                                                if ( columnSettings.hidden.md ) {
                                                        theadTh.addClass( "hidden-md" );
                                                }
                                                if ( columnSettings.hidden.lg ) {
                                                        theadTh.addClass( "hidden-lg" );
                                                }
                                                if ( columnSettings.hidden.print ) {
                                                        theadTh.addClass( "hidden-print" );
                                                }
                                        }
                                }
                        } else if ( maybe === false ) {
                                // Mark as not shown.
                                _headerShown = false;
                                // Clear the header.
                                _theadTr.empty();
                        }
                        return _that;
                };
                this.footer = function ( maybe ) {
                        // Show or redraw it.
                        if ( maybe === undefined || maybe === true ) {
                                // Mark as shown.
                                _footerShown = true;
                                // Clear the footer.
                                _tfootTr.empty();
                                for ( var i = 0; i < _columnsArray.length; i++ ) {
                                        var columnId = _columnsArray[ i ];
                                        var columnSettings = _columnsMap[ columnId ];
                                        var tfootTh = $( "<th></th>" ).appendTo( _tfootTr );
                                        tfootTh.data( "columnId", columnId );
                                        tfootTh.text( columnSettings.name );
                                        if ( $.isPlainObject( columnSettings.hidden ) ) {
                                                if ( columnSettings.hidden.xs ) {
                                                        tfootTh.addClass( "hidden-xs" );
                                                }
                                                if ( columnSettings.hidden.sm ) {
                                                        tfootTh.addClass( "hidden-sm" );
                                                }
                                                if ( columnSettings.hidden.md ) {
                                                        tfootTh.addClass( "hidden-md" );
                                                }
                                                if ( columnSettings.hidden.lg ) {
                                                        tfootTh.addClass( "hidden-lg" );
                                                }
                                                if ( columnSettings.hidden.print ) {
                                                        tfootTh.addClass( "hidden-print" );
                                                }
                                        }
                                }
                        } else if ( maybe === false ) {
                                // Mark as not shown.
                                _footerShown = false;
                                // Clear the footer.
                                _tfootTr.empty();
                        }
                        return _that;
                };
                this.row = function ( param1, param2 ) {
                        // Add table rows, an array of {rowId: ..., rowData: ...}.
                        if ( $.isArray( param1 ) ) {
                                var tableRows = param1;
                                for ( var i = 0; i < tableRows.length; i++ ) {
                                        var row = tableRows[ i ];
                                        var rowId = row.rowId;
                                        var rowData = row.rowData;
                                        _that.row( rowId, rowData );
                                }
                        // First param is an object that must have an 'id' property.
                        } else if ( $.isPlainObject( param1 ) && param2 === undefined ) {
                                return _that.row( param1.id, param1 );
                        // First param is a string row id.
                        } else if ( typeof param1 === "string" ) {
                                var rowId = param1;
                                // Setting a row.
                                if ( $.isPlainObject( param2 ) ) {
                                        var rowData = param2;
                                        _rowsData[ rowId ] = rowData;
                                        var row = $( "<tr></tr>" ).appendTo( _tBody );
                                        row.data( "rowId", rowId );
                                        row.attr( 'tabindex', 0 );
                                        for ( var i = 0; i < _columnsArray.length; i++ ) {
                                                var columnId = _columnsArray[ i ];
                                                var columnSettings = _columnsMap[ columnId ];
                                                var cell = $( "<td></td>" ).appendTo( row );
                                                cell.data( "columnId", columnId );
                                                cell.attr( 'tabindex', 0 );
                                                var cellContent = undefined;
                                                if ( columnSettings.getter ) {
                                                        if ( $.isFunction( columnSettings.getter ) ) {
                                                                // The data getter has the table as "this".
                                                                columnSettings.getter.call( _that, cell, rowId, rowData, row, columnId );
                                                        } else {
                                                                cellContent = rowData[ columnSettings.getter ]; 
                                                                cell.data( "cellData", cellContent );
                                                                cell.text( cellContent );
                                                        }
                                                }
                                                if ( $.isPlainObject( columnSettings.hidden ) ) {
                                                        if ( columnSettings.hidden.xs ) {
                                                                cell.addClass( "hidden-xs" );
                                                        }
                                                        if ( columnSettings.hidden.sm ) {
                                                                cell.addClass( "hidden-sm" );
                                                        }
                                                        if ( columnSettings.hidden.md ) {
                                                                cell.addClass( "hidden-md" );
                                                        }
                                                        if ( columnSettings.hidden.lg ) {
                                                                cell.addClass( "hidden-lg" );
                                                        }
                                                        if ( columnSettings.hidden.print ) {
                                                                cell.addClass( "hidden-print" );
                                                        }
                                                }
                                        }
                                        _onRowAddedCallbacks.fire( rowId, rowData );
                                        return _that;
                                // Getting a row by id.
                                } else if ( param2 === undefined ) {
                                        var rows = _tBody.find( 'tr' ).filter( function () {
                                                return Number( $( this ).data( "rowId" ) ) === rowId;
                                        });
                                        return rows.first();
                                // Getting a cell by column id.
                                } else if ( typeof param2 === "string" ) {
                                        return _that.row( rowId ).find( 'td' ).filter( function () {
                                                return $( this ).data( "columnId" ) === param2;
                                        });
                                // Remove row.
                                } else if ( param2 === null ) {
                                        _that.row( rowId ).remove();
                                        var rowData = _rowsData[ rowId ];
                                        delete _rowsData[ rowId ];
                                        _onRowRemovedCallbacks.fire( rowId, rowData );
                                        return _that;
                                // Do nothing.
                                } else {
                                        return _that;
                                }
                        }
                        return _that;
                };
                this.onRowAdded = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _onRowAddedCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.onRowRemoved = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _onRowRemovedCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                // Receives a callback with params: the JQ row and the rowId.
                this.onRowClick = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _onRowClickCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                // Receives a callback with params: the JQ row and the rowId.
                this.onRowRightClick = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _onRowRightClickCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.data = function ( param ) {
                        // Get the table's data indexed by row ID.
                        if ( param === undefined ) {
                                return _rowsData;
                        // Get a row's data.
                        } else if ( typeof param === "string" ) {
                                var rowId = param;
                                return _rowsData[ rowId ];
                        }
                        return _that;
                };
                // Create the public instance methods.
                // Add or return the value of a filter function.
                this.filter = function ( filterName, filterFunction ) {
                        // Execute the given filter.
                        if ( filterFunction === undefined ) {
                                // Call it changing the "this" context to here.
                                return _filters[ filterName ].call( _that );
                        // Set the given filter.
                        } else {
                                _filters[ filterName ] = filterFunction;
                        }
                        return _that;
                };
                // Set the selector params or return an array with selected IDs.
                this.selector = function ( accumParam ) {
                        // No param, return an array with the selected elements.
                        if ( accumParam === undefined ) {
                                // Return IDs only.
                                var ans = [];
                                for ( var rowId in _selectorAccum ) {
                                        if ( rowId !== undefined ) {
                                                ans.push( rowId );
                                        }
                                }
                                return ans;
                        // Return this selected element data.
                        } else if ( typeof accumParam === "string" ) {
                                var rowId = accumParam;
                                return _selectorAccum[ rowId ];
                        // Set the selector settings.
                        } else if ( $.isPlainObject( accumParam ) ) {
                                _selectorSettings = accumParam;
                        // Clear the selection.
                        } else if ( accumParam === null ) {
                                _selectorAccum = {};
                                // TODO: Fire only if it changes!
                                _onSelectorChangedCallbacks.fire( _that );
                        }
                        /* TODO An array sets the selected elements.
                        } else if ( $.isArray( accumParam ) ) {
                                _selectorAccum = {};
                                for ( var i = 0; i < accumParam.length; i++ ) {
                                        var key = accumParam[ i ];
                                        if ( typeof key === 'number' ) {
                                                _selectorAccum[ key ] = true;
                                        }
                                }
                                // Selector changed, fire the event.
                                _onSelectorChangedCallbacks.fire( _that );
                        }
                        */
                        return _that;
                };
                // Callback on selector change.
                this.onSelectorChanged = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _onSelectorChangedCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.search = function ( searchParam ) {
                        // If no search param, get the search string.
                        if ( searchParam === undefined ) {
                                return _searchString;
                        // If search param is not undefined, search.
                        } else if ( typeof searchParam === "string" ) {
                                // If the search changed, save it and reload the table.
                                if ( searchParam !== _searchString ) {
                                        _searchString = searchParam;
                                        _onSearchChangedCallbacks.fire( _searchString );
                                }
                        }
                        return _that;
                };
                this.onSearchChanged = function( callback ) {
                        // Attach a callback with this bind to the Library.
                        _onSearchChangedCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.limit = function ( limitParam ) {
                        // If no limit param, get the limit.
                        if ( limitParam === undefined ) {
                                return _limitNumber;
                        // If limit param is a number select it.
                        } else if ( typeof limitParam === "number" ) {
                                if ( !isNaN( limitParam ) && isFinite( limitParam ) && limitParam > 0 ) {
                                        // If the limit changed, save it and fire change event.
                                        if ( limitParam !== _limitNumber ) {
                                                _limitNumber = limitParam;
                                                _onLimitChangedCallbacks.fire( _limitNumber );
                                        }
                                        // Store the initial parameter.
                                        if ( _introspectionTableName === undefined ) {
                                                _limitNumberBeforeLoad = limitParam;
                                        }
                                }
                        }
                        return _that;
                };
                this.onLimitChanged = function( callback ) {
                        // Attach a callback with this bind to the Library.
                        _onLimitChangedCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.offset = function ( offsetParam ) {
                        // If no offset param, get the offset.
                        if ( offsetParam === undefined ) {
                                return _offsetNumber;
                        // If offset param is a number set it.
                        } else if ( typeof offsetParam === "number" ) {
                                if ( !isNaN( offsetParam ) && isFinite( offsetParam ) && offsetParam >= 0 && ( _recordsFiltered === undefined || offsetParam < _recordsFiltered ) ) {
                                        // If the offset changed, save it and fire change event.
                                        if ( offsetParam !== _offsetNumber ) {
                                                _offsetNumber = offsetParam;
                                                _onOffsetChangedCallbacks.fire( _offsetNumber );
                                        }
                                        // Store the initial parameter.
                                        if ( _introspectionTableName === undefined ) {
                                                _offsetNumberBeforeLoad = offsetParam;
                                        }
                                }
                        }
                        return _that;
                };
                this.onOffsetChanged = function( callback ) {
                        // Attach a callback with this bind to the Library.
                        _onOffsetChangedCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.recordsTotal = function () {
                        return _recordsTotal;
                };
                this.onRecordsTotalChanged = function( callback ) {
                        // Attach a callback with this bind to the Library.
                        _onRecordsTotalChangedCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.recordsFiltered = function () {
                        return _recordsFiltered;
                };
                this.onRecordsFilteredChanged = function( callback ) {
                        // Attach a callback with this bind to the Library.
                        _onRecordsFilteredChangedCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.load = function ( introspectionTableName ) {
                        _loadUUID = _loadUUID + 1;
                        var uuid = _loadUUID;
                        // Change load source ??
                        var firstLoad = false;
                        if ( introspectionTableName !== undefined ) {
                                firstLoad = true;
                                _introspectionTableName = introspectionTableName;
                                _onLoadCallbacks.fire();
                        }
                        // The deferred to return.
                        var deferred = $.Deferred();
                        // Make the rows invisible.
                        // So the table has the same size but shows no data.
                        _tBody.find( 'tr' ).addClass( "invisible" );
                        // Load from server.
                        jsMVC.action.table( _introspectionTableName,
                                (function () {
                                        // The final object
                                        var request = {};
                                        // Pagination
                                        if ( _that.limit() !== undefined && _that.offset() !== undefined ) {
                                                request.tablePagination = {};
                                                // Pagination first record.
                                                request.tablePagination.paginationFirstRecord = _that.offset();
                                                // Pagination record count.
                                                request.tablePagination.paginationMaxRecords = _that.limit();
                                        } else {
                                                request.tablePagination = null;
                                        }
                                        // Search object.
                                        if ( _that.search() !== undefined ) {
                                                request.tableSearch = {};
                                                request.tableSearch.searchString = _that.search();
                                        } else {
                                                request.tableSearch = null;
                                        }
                                        // Filters.
                                        if ( ! $.isEmptyObject( _filters ) ) {
                                                request.tableFilters = {};
                                                for ( var filterName in _filters ) {
                                                        var filter = _filters[ filterName ];
                                                        // Run the filter.
                                                        request.tableFilters[ filterName ] = filter.call( _that );
                                                }
                                        } else {
                                                request.tableFilters = null;
                                        }
                                        // Table columns.
                                        request.tableColumns = {};
                                        // JSON
                                        return request;
                                })()
                        ).done ( function ( response ) {
                                // Is this the newest request in progress ??
                                if ( _loadUUID === uuid ) {
                                        // Save the last totals.
                                        if ( _recordsTotal !== response.recordsTotal ) {
                                                _recordsTotal = response.recordsTotal;
                                                _onRecordsTotalChangedCallbacks.fire( _recordsTotal );
                                        }
                                        if ( _recordsFiltered !== response.recordsFiltered ) {
                                                _recordsFiltered = response.recordsFiltered;
                                                _onRecordsFilteredChangedCallbacks.fire( _recordsFiltered );
                                        }
                                        // Load the new data.
                                        for ( var rowId in _rowsData ) {
                                                var rowData = _rowsData[ rowId ];
                                                delete _rowsData[ rowId ];
                                                _onRowRemovedCallbacks.fire( rowId, rowData );
                                        }
                                        // Empty the body.
                                        _tBody.empty();
                                        _rowsData = {};
                                        _that.row( response.tableRows );
                                        // Resolve the returned object with the Library as "this" and param.
                                        _loadDeferred.resolveWith( _that, [ _that ] );
                                        deferred.resolveWith( _that, [ _that ] );
                                        if ( firstLoad ) {
                                                _onLoadedCallbacks.fire();
                                        }
                                } else {
                                        // If a newer request will be resolved, don't trigger the 
                                        // callback of this one, but reject so the caller can get notice.
                                        _loadDeferred.rejectWith( _that, [ _that ] );
                                        deferred.rejectWith( _that, [ _that ] );
                                }
                        });
                        return deferred.promise();
                };
                this.onLoad = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _onLoadCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.onLoaded = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _onLoadedCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.loadDeferred = function () {
                        // TODO: Can I return the promise directly without 
                        // the function.
                        return _loadDeferred.promise();
                };
                this.reload = function () {
                        // Do not process a load in progress.
                        _loadUUID = _loadUUID + 1;
                        // Do nothing after clearing or before a first load.
                        if ( _introspectionTableName !== undefined ) {
                                _onReloadCallbacks.fire();
                                _that.load( undefined ).done( function () {
                                        _onReloadedCallbacks.fire();
                                });
                        }
                        return _that;
                };
                this.onReload = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _onReloadCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.onReloaded = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _onReloadedCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.clear = function () {
                        // Do not process a load in progress.
                        _loadUUID = _loadUUID + 1;
                        // Clear the table and its data (not columns, header or footers).
                        _tBody.empty();
                        _rowsData = {};
                        // Avoid reloads, load must be called first.
                        _introspectionTableName = undefined;
                        // Clean the properties/accumulators Variables.
                        _selectorAccum = {}; // Clear the selector but no its settings.
                        _searchString = undefined;
                        // Keep the initial pagination parameters.
                        _limitNumber = _limitNumberBeforeLoad;
                        _offsetNumber = _offsetNumberBeforeLoad;
                        _recordsTotal = undefined;
                        _recordsFiltered = undefined;
                        // Clear callbacks.
                        _onClearCallbacks.fire();
                        return _that;
                };
                this.onClear = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _onClearCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                // Destroy the table and its handlers.
                this.destroy = function () {
                        // Do not process a load in progress.
                        _loadUUID = undefined;
                        // First fire the destroy event to avoid handlers using undefined vars.
                        _onDestroyCallbacks.fire();
                        _onDestroyCallbacks.disable();
                        // Clear wthout firing the event.
                        _onClearCallbacks.disable();
                        _that.clear();
                        // Now destroy the table.
                        _table.empty();
                        // Clean all the variables.
                        _that = undefined;
                        _table = undefined;
                        _columnsArray = undefined;
                        _columnsMap = undefined;
                        _filters = undefined;
                        _selectorSettings = undefined;
                        _selectorAccum = undefined;
                        _limitNumberBeforeLoad = undefined;
                        _limitNumber = undefined;
                        _offsetNumberBeforeLoad = undefined;
                        _offsetNumber = undefined;
                        _loadDeferred = $.Deferred();
                        _onRowAddedCallbacks.disable();
                        _onRowRemovedCallbacks.disable();
                        _onRowClickCallbacks.disable();
                        _onRowRightClickCallbacks.disable();
                        _onSelectorChangedCallbacks.disable();
                        _onSearchChangedCallbacks.disable();
                        _onLimitChangedCallbacks.disable();
                        _onOffsetChangedCallbacks.disable();
                        _onRecordsTotalChangedCallbacks.disable();
                        _onRecordsFilteredChangedCallbacks.disable();
                        _onLoadCallbacks.disable();
                        _onLoadedCallbacks.disable();
                        _onReloadCallbacks.disable();
                        _onReloadedCallbacks.disable();
                        // No chainable return.
                        return undefined;
                };
                this.onDestroy = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _onDestroyCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.widget = function ( options ) {
                        if ( $.isPlainObject( options ) ) {
                                // Click to reload.
                                if ( options.reloader ) {
                                        // Hidden by default, until loaded.
                                        $( options.reloader ).addClass( "hidden" );
                                        // Loading handlers.
                                        _that.onLoad( function () {
                                                $( options.reloader ).addClass( "hidden" );
                                        });
                                        _that.onLoaded( function () {
                                                $( options.reloader ).removeClass( "hidden" );
                                        });
                                        _that.onReload( function () {
                                                $( options.reloader ).addClass( "hidden" );
                                        });
                                        _that.onReloaded( function () {
                                                $( options.reloader ).removeClass( "hidden" );
                                        });
                                        // Clear handler.
                                        _that.onClear( function () {
                                                $( options.reloader ).addClass( "hidden" );
                                        });
                                        // Destroy handler.
                                        _that.onDestroy( function () {
                                                $( options.reloader ).addClass( "hidden" );
                                        });
                                        $( options.reloader ).on( 'click', function () {
                                                _that.reload();
                                        });
                                }
                                if ( options.loadingSpinner ) {
                                        // Hidden by default, until loading.
                                        $( options.loadingSpinner ).addClass( "hidden" );
                                        // Loading handlers.
                                        _that.onLoad( function () {
                                                $( options.loadingSpinner ).removeClass( "hidden" );
                                        });
                                        _that.onLoaded( function () {
                                                $( options.loadingSpinner ).addClass( "hidden" );
                                        });
                                        _that.onReload( function () {
                                                $( options.loadingSpinner ).removeClass( "hidden" );
                                        });
                                        _that.onReloaded( function () {
                                                $( options.loadingSpinner ).addClass( "hidden" );
                                        });
                                        // Clear handler.
                                        _that.onClear( function () {
                                                $( options.loadingSpinner ).addClass( "hidden" );
                                        });
                                        // Destroy handler.
                                        _that.onDestroy( function () {
                                                $( options.loadingSpinner ).addClass( "hidden" );
                                        });
                                }
                                // Show the total number of rows.
                                if ( options.totalCounter ) {
                                        // Its container.
                                        if ( options.totalCounterContainer ) {
                                                // Hidden by default, until loaded.
                                                $( options.totalCounterContainer ).addClass( "hidden" );
                                                // Loading handlers.
                                                _that.onLoaded( function () {
                                                        $( options.totalCounterContainer ).removeClass( "hidden" );
                                                });
                                                _that.onReload( function () {
                                                        $( options.totalCounterContainer ).addClass( "hidden" );
                                                });
                                                _that.onReloaded( function () {
                                                        $( options.totalCounterContainer ).removeClass( "hidden" );
                                                });
                                                // Clear handler.
                                                _that.onClear( function () {
                                                        $( options.totalCounterContainer ).addClass( "hidden" );
                                                });
                                                // Destroy handler.
                                                _that.onDestroy( function () {
                                                        $( options.totalCounterContainer ).addClass( "hidden" );
                                                });
                                        }
                                        // Set the default.
                                        var defaultValue = _that.recordsTotal();
                                        if ( defaultValue === undefined ) {
                                                $( options.totalCounter ).text( "" );
                                        } else {
                                                $( options.totalCounter ).text( defaultValue );
                                        }
                                        // Loading handlers.
                                        _that.onReload( function () {
                                                $( options.totalCounter ).addClass( "invisible" );
                                        });
                                        _that.onReloaded( function () {
                                                $( options.totalCounter ).removeClass( "invisible" );
                                        });
                                        // Change handler.
                                        _that.onRecordsTotalChanged( function ( recordsTotal ) {
                                                $( options.totalCounter ).text( recordsTotal );
                                        });
                                        // Clear handler.
                                        _that.onClear( function () {
                                                $( options.totalCounter ).text( 0 );
                                        });
                                        // Destroy handler.
                                        _that.onDestroy( function () {
                                                $( options.totalCounter ).text( "" );
                                        });
                                }
                                // Show the number of rows after the filters are applied.
                                if ( options.filteredCounter ) {
                                        // Set the default.
                                        var defaultValue = _that.recordsFiltered();
                                        if ( defaultValue === undefined ) {
                                                $( options.filteredCounter ).text( "" );
                                        } else {
                                                $( options.filteredCounter ).text( defaultValue );
                                        }
                                        // Change handler.
                                        _that.onRecordsFilteredChanged( function ( recordsFiltered ) {
                                                if ( recordsFiltered === _that.recordsTotal() ) {
                                                        $( options.filteredCounter ).text( "" );
                                                } else {
                                                        $( options.filteredCounter ).text( recordsFiltered );
                                                }
                                        });
                                        // Clear handler.
                                        _that.onClear( function () {
                                                $( options.filteredCounter ).text( "" );
                                        });
                                        // Destroy handler.
                                        _that.onDestroy( function () {
                                                $( options.filteredCounter ).text( "" );
                                        });
                                }
                                // Show the number of selected rows.
                                if ( options.selectorCounter ) {
                                        // Set the default.
                                        var defaultValue = _that.selector().length;
                                        if ( defaultValue === 0 ) {
                                                $( options.selectorCounter ).addClass( "hidden" );
                                                $( options.selectorCounter ).text( "" );
                                        } else {
                                                $( options.selectorCounter ).removeClass( "hidden" );
                                                $( options.selectorCounter ).text( defaultValue );
                                        }
                                        // Change handler.
                                        _that.onSelectorChanged( function () {
                                                var selector = _that.selector();
                                                if ( selector.length === 0 ) {
                                                        $( options.selectorCounter ).addClass( "hidden" );
                                                        $( options.selectorCounter ).text( "" );
                                                } else {
                                                        $( options.selectorCounter ).removeClass( "hidden" );
                                                        $( options.selectorCounter ).text( selector.length );
                                                }
                                        });
                                        // Show a popover to cancel the actual selection.
//              var clearForm = $( "<form class='form-inline'></form>" );
//              var clearFormGroup = $( "<div class='form-group'></form>" );
//              var clearFormLabel = $( "<label>Borrar</label>" )
                                        var clearFormButton = $( "<button class='btn btn-default'>Borrar</button>" );
                                        clearFormButton.on( 'click', function () {
                                                _that.selector( null );
                                                $( options.selectorCounter ).popover('hide');
                                        });
                                        $( options.selectorCounter ).addClass( "popover-container" );
                                        $( options.selectorCounter ).popover({
                                                container: 'body',
                                                content: clearFormButton,
                                                html: true,
                                                placement: 'below',
                                                template: '<div class="popover" role="tooltip"><div class="arrow"></div><h3 class="popover-title"></h3><div class="popover-content"></div></div>',
                                                title: "Seleccion",
                                                trigger: 'click'
                                        });
                                        // Clear handler.
                                        _that.onClear( function () {
                                                $( options.selectorCounter ).text( "" );
                                        });
                                        // Destroy handler.
                                        _that.onDestroy( function () {
                                                $( options.selectorCounter ).text( "" );
                                        });
                                }
                                // Handle the search.
                                if ( options.searcher ) {
                                        // Show it so it can be hidden by default.
                                        $( options.searcher ).removeClass( "hidden" );
                                        $( options.searcher ).addClass( "visible" );
                                        // Search cancel button.
                                        if ( options.searcherCancel ) {
                                                $( options.searcherCancel ).on( 'click', function () {
                                                        _that.search( "" );
                                                });
                                                _that.onDestroy( function () {
                                                        $( options.searcherCancel ).off( 'click' );
                                                });
                                        }
                                        // Search cancel button container.
                                        if ( options.searcherCancelContainer ) {
                                                options.searcherCancelContainer.addClass( "hidden" );
                                                _that.onSearchChanged( function ( searchValue ) {
                                                        if ( typeof searchValue === "string" && searchValue !== "" ) {
                                                                options.searcherCancelContainer.removeClass( "hidden" );
                                                        } else {
                                                                options.searcherCancelContainer.addClass( "hidden" );
                                                        }
                                                });
                                        }
                                        // Set the default.
                                        $( "input", options.searcher ).prop( 'disabled', false );
                                        var defaultValue = _that.search();
                                        if ( defaultValue === undefined ) {
                                                $( "input", options.searcher ).val( "" );
                                        } else {
                                                $( "input", options.searcher ).val( defaultValue );
                                        }
                                        // Loading handlers.
                                        _that.onReload( function () {
                                                $( "input", options.searcher ).prop( 'disabled', true );
                                        });
                                        _that.onReloaded( function () {
                                                $( "input", options.searcher ).prop( 'disabled', false );
                                        });
                                        // Change handler.
                                        $( "input", options.searcher ).on( 'change', function () {
                                                _that.search( $( this ).val().trim() );
                                        });
                                        _that.onSearchChanged( function ( searchValue ) {
                                                $( "input", options.searcher ).val( typeof searchValue === "string" ? searchValue : "" );
                                                _that.offset( 0 );
                                                _that.reload();
                                        });
                                        // Clear handler.
                                        _that.onClear( function () {
                                                $( "input", options.searcher ).val( "" );
                                        });
                                        // Destroy handler.
                                        _that.onDestroy( function () {
                                                $( "input", options.searcher ).off( 'change' );
                                                $( "input", options.searcher ).prop( 'disabled', true );
                                                $( "input", options.searcher ).val( "" );
                                        });
                                }
                                // Handle the limit.
                                if ( options.limiter ) {
                                        // Show it so it can be hidden by default.
                                        $( options.limiter ).removeClass( "hidden" );
                                        $( options.limiter ).addClass( "visible" );
                                        // Set the first element of the select as the default.
                                        $( "select", options.limiter ).prop( 'disabled', false );
                                        var defaultValue = Number( $( "select", options.limiter ).val() );
                                        _that.limit( defaultValue );
                                        // Loading handlers.
                                        _that.onReload( function () {
                                                $( "select", options.limiter ).prop( 'disabled', true );
                                        });
                                        _that.onReloaded( function () {
                                                $( "select", options.limiter ).prop( 'disabled', false );
                                        });
                                        // Change handler.
                                        $( "select", options.limiter ).on( 'change', function () {
                                                var oldLimit = _that.limit( Number( $( this ).val() ) );
                                        });
                                        _that.onLimitChanged( function () {
                                                _that.reload();
                                        });
                                        // Clear handler.
                                        _that.onClear( function () {
                                                $( "select", options.limiter ).val( defaultValue );
                                                _that.limit( defaultValue );
                                        });
                                        // Destroy handler.
                                        _that.onDestroy( function () {
                                                $( "select", options.limiter ).off( 'change' );
                                                $( "select", options.limiter ).prop( 'disabled', true );
                                                $( "select", options.limiter ).val( defaultValue );
                                        });
                                }
                                // Paginators previous and next.
                                if ( options.offsetterPrev && options.offsetterNext ) {
                                        // Start hidden.
                                        $( options.offsetterPrev ).addClass( 'hidden' );
                                        $( options.offsetterNext ).addClass( 'hidden' );
                                        // Click handlers.
                                        var offsetterPrevClickHandler = function ( event ) {
                                                if ( _that.recordsFiltered() === undefined || _that.limit() === undefined || _that.offset() === undefined || _that.recordsFiltered() <= _that.limit() ) {
                                                        event.preventDefault();
                                                } else {
                                                        var limit = _that.offset() - _that.limit();
                                                        _that.offset( limit < 0 ? 0 : limit );
                                                        _that.reload();
                                                }
                                        };
                                        $( options.offsetterPrev ).on( 'click', offsetterPrevClickHandler );
                                        //$( options.offsetterPrev ).on( 'dblclick', offsetterPrevClickHandler );
                                        var offsetterNextClickHandler = function ( event ) {
                                                if ( _that.recordsFiltered() === undefined || _that.limit() === undefined || _that.offset() === undefined || _that.recordsFiltered() <= _that.limit() ) {
                                                        event.preventDefault();
                                                } else {
                                                        var limit = _that.offset() + _that.limit();
                                                        _that.offset( limit >= _that.recordsFiltered() ? ( _that.recordsFiltered() - 1 ) : limit );
                                                        _that.reload();
                                                }
                                        };
                                        $( options.offsetterNext ).on( 'click', offsetterNextClickHandler );
                                        //$( options.offsetterNext ).on( 'dblclick', offsetterNextClickHandler );
                                        // Loading handlers.
                                        _that.onReload( function () {
                                                $( options.offsetterPrev ).addClass( 'invisible' );
                                                $( options.offsetterNext ).addClass( 'invisible' );
                                        });
                                        // On change handlers.
                                        var callback = function () {
                                                if ( _that.recordsFiltered() === undefined || _that.limit() === undefined || _that.offset() === undefined || _that.recordsFiltered() <= _that.limit() ) {
                                                        $( options.offsetterPrev ).addClass( 'hidden' );
                                                        $( options.offsetterNext ).addClass( 'hidden' );
                                                        $( options.offsetterPrev ).removeClass( 'invisible' );
                                                        $( options.offsetterNext ).removeClass( 'invisible' );
                                                } else {
                                                        $( options.offsetterPrev ).removeClass( 'hidden' );
                                                        $( options.offsetterNext ).removeClass( 'hidden' );
                                                        // Prev
                                                        if ( _that.offset() <= 0 ) {
                                                                $( options.offsetterPrev ).addClass( 'invisible' );
                                                        } else {
                                                                $( options.offsetterPrev ).removeClass( 'invisible' );
                                                        }
                                                        // Next
                                                        if ( _that.offset() + _that.limit() >= _that.recordsFiltered() ) {
                                                                $( options.offsetterNext ).addClass( 'invisible' );
                                                        } else {
                                                                $( options.offsetterNext ).removeClass( 'invisible' );
                                                        }
                                                }
                                        };
                                        _that.onReloaded( callback );
                                        _that.onOffsetChanged( callback );
                                        _that.onLimitChanged( callback );
                                        _that.onRecordsFilteredChanged( callback );
                                        // Destroy handler.
                                        _that.onDestroy( function () {
                                                $( options.offsetterPrev ).off( 'click' );
                                                $( options.offsetterNext ).off( 'click' );
                                                $( options.offsetterPrev ).addClass( 'hidden' );
                                                $( options.offsetterNext ).addClass( 'hidden' );
                                        });
                                }
                                // Paginators numbers.
                                if ( options.offsetterMiddle ) {
                                        // How many pagers?
                                        var size = $( options.offsetterMiddle ).size();
                                        // Start hidden.
                                        $( options.offsetterMiddle ).addClass( 'hidden' );
                                        // Loading handlers.
                                        _that.onReload( function () {
                                                $( options.offsetterMiddle ).addClass( 'invisible' );
                                        });
                                        // Click handlers.
                                        // Using find searches on the childs of each matched elements.
                                        $( options.offsetterMiddle ).find( 'a' ).each( function ( index, elem ) {
                                                $( elem ).on( 'click', function ( event ) {
                                                        if ( index === (size/2)|| _that.recordsFiltered() === undefined || _that.limit() === undefined || _that.offset() === undefined || _that.recordsFiltered() <= _that.limit() ) {
                                                                event.preventDefault();
                                                        } else {
                                                                var offset = Math.floor( index * _that.recordsFiltered() / size );
                                                                _that.offset( offset >= _that.recordsFiltered() ? ( _that.recordsFiltered() - 1 ) : offset );
                                                                _that.reload();
                                                        }
                                                });
                                        });
                                        // On change handlers.
                                        var callback = function () {
                                                if ( _that.recordsFiltered() === undefined || _that.limit() === undefined || _that.offset() === undefined || _that.recordsFiltered() <= _that.limit() ) {
                                                        $( options.offsetterMiddle ).addClass( 'hidden' );
                                                } else {
                                                        $( options.offsetterMiddle ).removeClass( 'hidden' );
                                                        $( options.offsetterMiddle ).each( function ( index, elem ) {
                                                                $( elem ).removeClass( 'invisible' );
                                                                if ( !(index === (size/2)) ) {
                                                                        var offset = Math.floor( index * (_that.recordsFiltered() / size) );
                                                                        var offsetText = offset + 1;
                                                                        //$( elem ).find( 'a' ).text( offsetText );
                                                                        if ( ( offset === _that.offset() ) ) {
                                                                                $( elem ).addClass( 'active' );
                                                                        } else {
                                                                                $( elem ).removeClass( 'active' );
                                                                        }
                                                                }
                                                        });
                                                }
                                        };
                                        _that.onReloaded( callback );
                                        _that.onOffsetChanged( callback );
                                        _that.onLimitChanged( callback );
                                        _that.onRecordsFilteredChanged( callback );
                                        // Destroy handler.
                                        _that.onDestroy( function () {
                                                $( options.offsetterMiddle ).find( 'a' ).off( 'click' );
                                                $( options.offsetterMiddle ).addClass( 'hidden' );
                                        });
                                }
                                if ( options.offsetterPrev || options.offsetterNext || options.offsetterMiddle ) {
                                        // Star on zero.
                                        _that.offset( 0 );
                                        // Clear handler.
                                        _that.onClear( function () {
                                                _that.offset( 0 );
                                        });
                                }
                        }
                        return _that;
                };
                // Attach handlers.
                //----------------------------------------------------------------------
                // Attach onRowClick handlers.
                var onRowClick = function ( event ) {
                        // Left click only.
                        if ( event.which === 1 ) {
                                var jqRow = $( this );
                                var rowId = jqRow.data( "rowId" );
                                // When the table is empty a row telling "no data" appears.
                                if ( rowId !== undefined ) {
                                        _onRowClickCallbacks.fire( rowId, jqRow );
                                }
                        }
                };
                _tBody.on( 'click', 'tr', onRowClick );
                _that.onDestroy( function () { 
                        _tBody.off( 'click', 'tr', onRowClick ); 
                } );
                // Attach onRowRightClick handlers.
                var onRowRightClick = function ( event ) {
                        // Right click only and don't prevent default if no callabcks.
                        if ( event.which === 3 && _onRowRightClickCallbacks.has() ) {
                                event.preventDefault();
                                var jqRow = $( this );
                                var rowId = jqRow.data( "rowId" );
                                // When the table is empty a row telling "no data" appears.
                                if ( rowId !== undefined ) {
                                        _onRowRightClickCallbacks.fire( event, jqRow, rowId );
                                }
                        }
                };
                _tBody.on( 'contextmenu', 'tr', onRowRightClick );
                _that.onDestroy( function () { 
                        _tBody.off( 'contextmenu', 'tr', onRowRightClick ); 
                } );
                // Attach the selector callbacks.
                var markSelectedRows = function () {
                        _tBody.find( 'tr' ).each( function () {
                                var row = $( this );
                                var rowId = row.data( "rowId" );
                                if ( _selectorAccum[ rowId ] === undefined ) {
                                        row.removeClass( _selectorSettings.rowClass );
                                } else {
                                        row.addClass( _selectorSettings.rowClass );
                                }
                        });
                };
                _that.onSelectorChanged( function () {
                        markSelectedRows();
                });
                _that.onRowClick( function ( rowId, row ) {
                        if ( _selectorSettings !== undefined ) {
                                if ( row.hasClass( _selectorSettings.rowClass ) ) {
                                        delete _selectorAccum[ rowId ];
                                } else {
                                        if ( _selectorSettings.multiRow === false ) {
                                                _selectorAccum = {};
                                        }
                                        _selectorAccum[ rowId ] = _that.data( rowId );
                                }
                                _onSelectorChangedCallbacks.fire( _that );
                        }
                });
                _that.onReloaded( function () {
                        if ( _selectorSettings !== undefined ) {
                                var selectorChanged = false;
                                for ( var rowId in _selectorAccum ) {
                                        if ( rowId !== undefined ) {
                                                if ( _selectorSettings.multiPage === false ) {
                                                        // Just delete the ones not on the redrawn page.
                                                        if ( _that.data( rowId ) === undefined ) {
                                                                delete _selectorAccum[ rowId ];
                                                                selectorChanged = selectorChanged || true;
                                                        }
                                                }
                                        }
                                }
                                if ( selectorChanged ) {
                                        _onSelectorChangedCallbacks.fire( _that );
                                } else {
                                        markSelectedRows();
                                }
                        }
                });
                // Attach on key press handlers.
                var onKeyPress = function ( event ) {
                        switch( event.which ) {
                                // Space
                                case 32:
                                        break;
                                // Left
                                case 37:
                                        break;
                                // Up
                                case 38:
                                        break;
                                // Rigth
                                case 39:
                                        break;
                                // Down
                                case 40:
                                        break;
                                // Others.
                                default:
                                        //alert( event.which );
                        }
                };
                _table.on( 'keyup', 'td', onKeyPress );
                _that.onDestroy( function () { 
                        _table.off( 'keyup', 'td', onKeyPress );
                });
        };
        // Return the jQuery like object.
        return Library;
})( this, this.document, jQuery );
