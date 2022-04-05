// Library module:
// http://javascript.crockford.com/private.html
// http://blog.buymeasoda.com/creating-a-jquery-like-chaining-api/

// TODO:
// Include jsMVC.translations and https://github.com/jquery/globalize

// jsMVC
/******************************************************************************/

var jsMVC = {};

// Pages
/******************************************************************************/

jsMVC.page = {};

jsMVC.page.name = undefined;

// A unique UUID for the pages.
jsMVC.page.uuid = 1;

jsMVC.page.nextUUID = function () {
        var ans = jsMVC.page.uuid;
        jsMVC.page.uuid = jsMVC.page.uuid + 1;
        return ans;
};

// Cache isSubPage because later the URL anchor or the parent window can change.
jsMVC.page.subPage = undefined;

// Are we on a jsMVC subpage.
jsMVC.page.isSubPage = function () {
        if ( jsMVC.page.subPage !== undefined ) {
                return jsMVC.page.subPage;
        // Check if another window started this one.
        } else {
                var ans = undefined;
                if ( jQuery.isWindow( window.opener ) === false ) {
                        ans = false;
                } else {
                        var params = undefined;
                        try { 
                                params = window.opener.jsMVC_page_params;
                        } catch ( err ) {
                                // Same origin policy errors.
                                // When popups are blocked on Chrome, it shows that a popup was 
                                // blocked with a link to it. When the user clicks the popup link, 
                                // the opened window is still subject to the same origin policy as 
                                // if it was a completely new window with Chrome as its parent.
                                // The error is: 
                                // Uncaught SecurityError: Blocked a frame with origin 
                                // "http://127.0.0.1:8888" from accessing a frame with origin 
                                // "swappedout://". The frame requesting access has a protocol of 
                                // "http", the frame being accessed has a protocol of "swappedout". 
                                // Protocols must match.
                                window.close();
                        }
                        // Is a jsMVC subpage? Look for the page parameters object.
                        if ( jQuery.isPlainObject( params ) === false ) {
                                ans = false;
                        // Now look for the page name on the URL anchor.
                        } else if ( jQuery.type( window.location.hash ) !== "string" || window.location.hash === "" || window.location.hash === "#" || window.location.hash.charAt( 0 ) !== "#" ) {
                                ans = false;
                        // Now check that the parameters for this page name exist.
                        } else if ( jQuery.isPlainObject( params[ window.location.hash.substr(1) ] ) === false ) {
                                ans = false;
                        } else {
                                ans = true;
                        }
                }
                jsMVC.page.subPage = ans;
                return ans;
        }
};

jsMVC.page.cleanPath = function ( pathName ) {
        if ( pathName === "" || pathName.charAt( 0 ) !== "/" ) {
                pathName = "/" + pathName;
        }
        if ( pathName !== "/" && pathName.charAt( pathName.length - 1 ) === "/" ) {
                pathName = pathName.substring( 0, pathName.length - 2 );
        }
        return pathName;
};

// When this is the first page or for landing pages (a completely new jsMVC 
// "instance" with no reference to any other page) the view, controller and 
// parameters are fetched from the server for the actual URL. If something 
// fails it loads the "error" view and controller with no parameters.
// When opening a sub-page or child-page (calling jsMVC.page.open) all the view 
// parameters are supplied by the parent.
jsMVC.page.load = function ( viewContainerSelector ) {
        // The deferred to return.
        var deferred = jQuery.Deferred();
        // Store the page name.
        jsMVC.page.name = jsMVC.page.cleanPath( window.location.pathname );
        // If not on a subpage, look for the page parameters by URL.
        if ( jsMVC.page.isSubPage() === false ) {
                var pageName = window.location.pathname;
                //TODO: pageName = pageName.substring( jsMVC.page.name.length , pageName.length );
                jsMVC.action.page( jsMVC.page.cleanPath( pageName ), null )
                // The server returned the view and controller to use.
                .done( function ( page ) {
                        jsMVC.render(
                                viewContainerSelector,
                                page.pageResponseView,
                                page.pageResponseController,
                                page.pageResponseParams
                        ).done( function () {
                                // New page rendering finished.
                                deferred.resolve();
                        });
                // If the lookup fails, load the ones on the HTML tags.
                }).fail( function () {
                        jsMVC.render( viewContainerSelector, "error", "error" )
                        .done( function () {
                                // New page rendering finished.
                                deferred.resolve();
                        });
                });
        // If on a subpage get the page parameters from the parent window.
        } else {
                var params = window.opener.jsMVC_page_params[ window.location.hash.substr(1) ];
                // Restore the intended anchor.
                window.location.hash = params.hashString;
                // Leave a reference of this subwindow on the parent.
                params.window = window;
                // TODO: Make a deep copy of the parameters.
                // I don't know what happens when the parent closes.
                // But what happens if the parameters include callbacks.
                jsMVC.render(
                        viewContainerSelector,
                        params.viewName,
                        params.controllerName,
                        params.controllerParams
                ).done( function () {
                        // Resolve the parent window deferred.
                        if ( params.deferred !== undefined ) {
                                params.deferred.resolve();
                        }
                        // New page rendering finished.
                        deferred.resolve();
                });
        }
        // Return the promise only.
        return deferred.promise();
};

// Open a new page for the given URL.
jsMVC.page.open = function ( pageName, viewName, controllerName, controllerParams ) {
        // The deferred to return.
        var deferred = jQuery.Deferred();
        if ( jQuery.type( pageName ) === "string" && pageName !== "" ) {
                if ( pageName.charAt( 0 ) !== "/" ) {
                        pageName = "/" + pageName;
                }
                var pageParamsPropertyName = "jsMVC-subpage-" + jsMVC.page.nextUUID();
                window.jsMVC_page_params[ pageParamsPropertyName ] = {
                        // Placeholder for the new window.
                        window: undefined,
                        // The deferred will be resolved by the new window.
                        deferred: deferred,
                        // The anchor to replace on the new window.
                        hashString: "",
                        // The rendering parameters.
                        viewName: viewName,
                        controllerName: controllerName,
                        controllerParams: controllerParams
                };
                var ans = window.open( pageName + "#" + pageParamsPropertyName, "_blank" );
                if ( ans === undefined || jQuery.isWindow( ans ) === false ) {
                        deferred.reject();
                }
        }
        // Return the promise only.
        return deferred.promise();
};

window.jsMVC_page_params = {};

// When this window is about to close, close all it subwindows.
window.onbeforeunload = function () {
        for ( var key in window.jsMVC_page_params ) {
                var params = window.jsMVC_page_params[ key ];
                if ( jQuery.isWindow( params.window ) === true && jQuery.isFunction( params.window.close ) === true ) {
                        params.window.close();
                }
        };
        return null;
};

// Controllers
/******************************************************************************/

// Load javascripts.
jsMVC.controller = {};

// The deferrs of the views that are downloading or downloaded.
jsMVC.controller.queue = {};

// Load and execute a script, returns a deferred object. If the second parameter is True the execution is blocking!
// TODO: Separate download from execution, see controlJS, from Steve Souders website.
// When a script is added the browser blocks until it is downloaded, parsed and ran.
// It's part of the specification to avoid dependency conclicts.
jsMVC.controller.load = function ( scriptName, sync ) {
        // The deferred to return.
        var deferred = jQuery.Deferred();
        // Get the download deferred from the queue.
        var downloader = jsMVC.controller.queue[ scriptName ];
        // First time downloading ??
        if (downloader === undefined) {
                // Create the downloader.
                downloader = jQuery.ajax({
                        url: "/_/s/controller/" + scriptName,
                        async: sync ? false : true,
                        // Don't append the timestamp.
                        cache: true,
                        // Create a script element that can be debugged.
                        // With this a script element is created so the source is shown.
                        crossDomain: true,
                        dataType: 'script',
                        mimeType: "text/javascript",
                        scriptCharset: "utf-8"
                });
                // Fill the queue.
                jsMVC.controller.queue[ scriptName ] = downloader;
        }
        // On download done.
        downloader.done( function ( stringScript ) {
                deferred.resolve( stringScript );
        });
        // On download fail.
        downloader.fail(function ( jqXHR, textStatus, errorThrown ) {
                // Clear queue.
                jsMVC.controller.queue[ scriptName ] = undefined;
                // Show alert.
                jQuery( "#jsMVC-collapse" ).collapse( 'show' );
                // TODO: Better error message. Use debug mode and apply styles, put a message when dbl click on the view, etc.
                deferred.reject( jqXHR, textStatus, errorThrown );
        });
        // Return the promise only.
        return deferred.promise();
};

// Views
/******************************************************************************/

// Load views.
jsMVC.view = {};

// The deferrs of the views that are downloading or downloaded.
jsMVC.view.queue = {};

// A unique UUID for the views.
jsMVC.view.uuid = 0;

jsMVC.view.nextUUID = function () {
        var ans = jsMVC.view.uuid;
        jsMVC.view.uuid = jsMVC.view.uuid + 1;
        return ans;
};

// Load the view from the server and returns the raw view (String) as a 
// parameter to the done callback of the returned deferred object.
// TODO: Allow to load HTML from a different server (cross domain). Maybe using 
// an iframe, I don't know.
jsMVC.view.load = function ( viewName ) {
        // The deferred to return.
        var deferred = jQuery.Deferred();
        // Get the download deferred from the queue.
        var downloader = jsMVC.view.queue[ viewName ];
        // First time downloading ??
        if ( downloader === undefined ) {
                // Create the downloader.
                downloader = jQuery.ajax({
                        url: "/_/s/view/" + viewName,
                        // Don't append the timestamp.
                        cache: true,
                        // Returns HTML as plain text; 
                        // Included script tags are evaluated when inserted in the DOM.
                        dataType: "html",
                        mimeType: "text/html"
                });
                // Fill the queue.
                jsMVC.view.queue[ viewName ] = downloader;
        }
        // On download done.
        downloader.done( function ( stringView ) {
                deferred.resolve( stringView );
        });
        // On download fail.
        downloader.fail( function ( jqXHR, textStatus, errorThrown ) {
                // Clear queue.
                jsMVC.view.queue[ viewName ] = undefined;
                // Show alert.
                jQuery( "#jsMVC-collapse" ).collapse( 'show' );
                // TODO: Better error message. Use debug mode and apply styles, put a message when dbl click on the view, etc.
                deferred.reject( jqXHR, textStatus, errorThrown );
        });
        // Return the promise only.
        return deferred.promise();
};

jsMVC.view.controller = function ( controllerName, controllerFunction ) {
        // Check controllerName parameter.
        if ( controllerName !== undefined && controllerName !== null && typeof controllerName === 'string' && controllerName !== "" ) {
                // Check controllerFunction parameter.
                if ( controllerFunction !== undefined && controllerFunction !== null && jQuery.isFunction( controllerFunction ) ) {
                        // Check if there is already a controller with that name.
                        if ( jsMVC.view.controller.queue[ controllerName ] === undefined ) {
                                jsMVC.view.controller.queue[ controllerName ] = controllerFunction;
                        }
                } else {
                        // If the parameters are not well formed mark it on the queue so the 
                        // controller is not loaded again.
                        jsMVC.view.controller.queue[ controllerName ] = null;
                }
        }
};

jsMVC.view.controller.queue = {};

// Returns the controller, a function.
jsMVC.view.controller.load = function ( controllerName ) {
        // The deferred to return.
        var deferred = jQuery.Deferred();
        // Check controllerName parameter.
        if ( controllerName !== undefined && controllerName !== null && typeof controllerName === 'string' && controllerName !== "" ) {
                // Load the controller as a JavaScript. Does nothing if already in cache.
                jsMVC.controller.load( controllerName ).done( function () {
                        // Get controller from queue.
                        var controllerFunction = jsMVC.view.controller.queue[ controllerName ];
                        // Call the controller if there is one.
                        if ( controllerFunction !== undefined && controllerFunction !== null && jQuery.isFunction( controllerFunction ) ) {
                                deferred.resolve( controllerFunction );
                        } else {
                                deferred.resolve( null ); // Or reject ??
                        }
                })
                .fail( function () {
                        deferred.resolve( null ); // Or reject ??
                });
        } else {
                deferred.resolve( null ); // Or reject ??
        }
        // Return the promise only.
        return deferred.promise();
};

// Images.
/******************************************************************************/

// Load images.
jsMVC.image = {};

// The deferrs of the images that are downloading or downloaded.
jsMVC.image.queue = {};

jsMVC.image.getUri = function ( uri ) {
        return "/_/s/html/images/" + uri;
};

jsMVC.image.load = function ( imageName ) {
        // The deferred to return.
        var deferred = jQuery.Deferred();
        // Get the download deferred from the queue.
        var downloader = jsMVC.image.queue[ imageName ];
        // First time downloading ??
        if ( downloader === undefined ) {
                // Create the downloader.
                var downloader = jQuery.Deferred();
                // URI to load.
                var uri = jsMVC.image.getUri( imageName );
                // Bind the image events to it.
                var image = jQuery( "<img>" );
                image.load( function () {
                        downloader.resolve( uri );
                });
                // Start downloading the image.
                image.attr( "src", uri );
                // Fill the queue.
                jsMVC.image.queue[ imageName ] = downloader;
        }
        // On download done.
        downloader.done( function ( uri ) {
                deferred.resolve( uri );
        });
        // On download fail.
        downloader.fail( function ( jqXHR, textStatus, errorThrown ) {
                // Clear queue.
                jsMVC.image.queue[ imageName ] = undefined;
                // TODO: Better error message.
                deferred.reject( jqXHR, textStatus, errorThrown );
        });
        // Return the promise only.
        return deferred.promise();
};

// Actions
/******************************************************************************/

jsMVC.action = {};

jsMVC.action.page = function ( pageName, pageParams ) {
        // The deferred to return.
        var deferred = jQuery.Deferred();
        jQuery.ajax( {
                url: "/_/d/page/",
                // Don't append the timestamp.
                cache: true,
                type: "post",
                contentType: 'application/json; charset=UTF-8',
                dataType: 'json',
                data: JSON.stringify( {pageRequestName: pageName, pageRequestParams: pageParams} )
        } ).done ( function ( data, textStatus, jqXHR ) {
                deferred.resolve( data );
        } ).fail ( function ( jqXHR, textStatus, errorThrown ) {
                // Show alert.
                jQuery( "#jsMVC-collapse" ).collapse( 'show' );
                deferred.reject( );
        } );
        // Return the promise only.
        return deferred.promise();
};

jsMVC.action.lookup = function ( object, json ) {
        // The deferred to return.
        var deferred = jQuery.Deferred();
        jQuery.ajax( {
                url: (jsMVC.page.name === "/" ? "" : jsMVC.page.name) + "/_/d/lookup/" + object,
                // Don't append the timestamp.
                cache: true,
                type: "post",
                contentType: 'application/json; charset=UTF-8',
                dataType: 'json',
                data: json === undefined ? undefined : JSON.stringify( json )
        } ).done ( function ( data, textStatus, jqXHR ) {
                deferred.resolve( data );
        } ).fail ( function ( jqXHR, textStatus, errorThrown ) {
                // Show alert.
                jQuery( "#jsMVC-collapse" ).collapse( 'show' );
                deferred.reject( );
        } );
        // Return the promise only.
        return deferred.promise();
};

jsMVC.action.post = function ( object, json ) {
        // The deferred to return.
        var deferred = jQuery.Deferred();
        jQuery.ajax( {
                url: "/_/d/post/" + object,
                // Don't append the timestamp.
                cache: true,
                type: "post",
                contentType: 'application/json; charset=UTF-8',
                dataType: 'json',
                data: json === undefined ? undefined : JSON.stringify( json )
        } ).done ( function ( data, textStatus, jqXHR ) {
                deferred.resolve( data );
        } ).fail ( function ( jqXHR, textStatus, errorThrown ) {
                // Show alert.
                jQuery( "#jsMVC-collapse" ).collapse( 'show' );
                deferred.reject( );
        } );
        // Return the promise only.
        return deferred.promise();
};

jsMVC.action.table = function ( object, json ) {
        // The deferred to return.
        var deferred = jQuery.Deferred();
        jQuery.ajax( {
                url: "/_/d/table/" + object,
                // Don't append the timestamp.
                cache: true,
                type: "post",
                contentType: 'application/json; charset=UTF-8',
                dataType: 'json',
                data: json === undefined ? undefined : JSON.stringify( json )
        } ).done ( function ( data, textStatus, jqXHR ) {
                deferred.resolve( data );
        } ).fail ( function ( jqXHR, textStatus, errorThrown ) {
                // Show alert.
                jQuery( "#jsMVC-collapse" ).collapse( 'show' );
                deferred.reject( );
        } );
        // Return the promise only.
        return deferred.promise();
};

// Render
/******************************************************************************/

// Render a section of the page by passing all its needed params.
jsMVC.render = function ( viewContainerSelector, viewName, controllerName, controllerParams ) {

        // The deferred to return.
        var deferred = jQuery.Deferred();

        // The view UUID.
        var uuid = jsMVC.view.nextUUID();
        // Add to the view container the class that marks it as a jsMVC view.
        jQuery( viewContainerSelector ).addClass( "jsMVC-view" );
        // Add also the view UUID to the container.
        jQuery( viewContainerSelector ).addClass( "jsMVC-view-" + uuid );

        // Start hidden until the content of this conainer is emptied.
        // This is because the parameters are inside and may be ugly to show.
        var isAlreadyHidden = jQuery( viewContainerSelector ).hasClass( "hidden" );
        jQuery( viewContainerSelector ).addClass( "hidden" );

        // Integration with bootstrap's tabs:
        // If container is a tab that is not the default one.
        var isTabHidden = jQuery( viewContainerSelector ).hasClass( 'tab-pane' ) && !jQuery( viewContainerSelector ).hasClass( 'in' );

        // Start downloading the needed view and controller.
        var viewDeferred = undefined;
        var controllerDeferred = undefined;
        // Deferred for this view subviews.
        var subviewsDeferred = jQuery.Deferred();
        if ( isTabHidden ) {
                // Load view controller in parallel when the tab starts showing.
                viewDeferred = jQuery.Deferred();
                controllerDeferred = jQuery.Deferred();
                // Attach handler that runs only once and removes itself.
                var href = jQuery( viewContainerSelector ).attr( 'id' );
                jQuery( 'a[href="#' + href + '"]' ).one( 'show.bs.tab', function () {
                        jsMVC.view.load( viewName ).done( function ( viewString ) {
                                viewDeferred.resolve( viewString );
                        });
                        jsMVC.view.controller.load( controllerName ).done( function ( controllerFunction ) {
                                controllerDeferred.resolve( controllerFunction );
                        });
                });
        } else {
                // Load the needed view controller in parallel.
                viewDeferred = jsMVC.view.load( viewName );
                controllerDeferred = jsMVC.view.controller.load( controllerName );
        }

        // The view parameters.
        var viewParamsValues = {};
        // The children function only travels a single level down the DOM tree.
        jQuery( viewContainerSelector ).children( ".jsMVC-view-param" ).each( function () {
                var paramElem = jQuery( this );
                var paramName = paramElem.attr( "data-jsMVC-view-param" );
                if ( paramName !== undefined && paramName !== null && typeof paramName === 'string' && paramName !== "" ) {
                        viewParamsValues[ paramName ] = paramElem.html();
                }
        });
        // After getting the parameters, clean the container content.
        jQuery( viewContainerSelector ).empty();
        // Now we can show it.
        if ( !isAlreadyHidden ) {
                jQuery( viewContainerSelector ).removeClass( "hidden" );
        }
        // Put a temporal "loading" view using a spinner.
        jQuery( viewContainerSelector ).html( "<img src='/_/s/html/images/jsMVC/spinner.gif'>" );

        // Resolve if tab because it is lazy loading.
        if ( isTabHidden ) {
                deferred.resolve();
        }

        // As soon as the view is ready show it and start rendering its subviews.
        viewDeferred.done( function ( viewString ) {
                // Include the view params.
                var paramsIncludedViewString = jsMVC.render.viewParams( viewString, viewParamsValues );
                // When view is ready alter the image tags so they don't start
                // downloading when inserted into the DOM.
                var editedViewString = jsMVC.render.alterImages( paramsIncludedViewString );
                // The view string was edited and every img tag was replaced
                // with a placeholder to change the download technique.
                jQuery( viewContainerSelector ).empty(); // Remove spinner.
                jQuery( viewContainerSelector ).html( editedViewString );
                // Disable view until controller is ready.
// TODO: A disabled div on top to show something while waiting.
// jQuery( viewContainerSelector ).addClass( "invisible" );
                var disablingDiv = '<div style="position:absolute;top:0;left:0;width:100%;height:100%;z-index:2;background:rgba(0, 0, 0, 0.5);"></div>';
// jQuery( viewContainerSelector ).wrap( disablingDiv );
                jQuery( viewContainerSelector ).append( disablingDiv );
                // After inserting into the DOM start downloading the images
                // asynchronously in parrallel.
                jsMVC.render.loadImages( viewContainerSelector );
                // Now start showing its subviews.
                jsMVC.render.html( viewContainerSelector ).then( function ( subViews ) {
                        // Mark subviews as parent of the actual view.
                        if ( subViews !== undefined && subViews !== null && jQuery.isArray( subViews ) ) {
                                for ( var key in subViews ) {
                                        var sv = subViews[ key ];
                                        if ( sv.containerSelector !== undefined ) {
                                                jQuery( sv.containerSelector ).addClass( "jsMVC-parentview-" + uuid );
                                        }
                                }
                        }
                        // When the subviews are show resolve its deferred.
                        subviewsDeferred.resolve( subViews );
                });
        });

        // When controller, view and subviews are ready.
        jQuery.when( viewDeferred, subviewsDeferred, controllerDeferred ).done( function ( viewString, subViews, controllerFunction ) {
                if ( controllerFunction !== undefined && controllerFunction !== null && jQuery.isFunction( controllerFunction ) ) {
                        // Add the class that marks it as having its own controller.
                        jQuery( viewContainerSelector ).addClass( "jsMVC-controller" );
                        // Add the controller UUID to the container.
                        jQuery( viewContainerSelector ).addClass( "jsMVC-controller-" + uuid );
                        // Mark subviews controllers as parent of the actual view controller.
                        if ( subViews !== undefined && subViews !== null && jQuery.isArray( subViews ) ) {
                                for ( var key in subViews ) {
                                        var sv = subViews[ key ];
                                        if ( sv.containerSelector !== undefined ) {
                                                // If it does not have its own controller, add parents controller UUID.
                                                if ( ! jQuery( sv.containerSelector ).hasClass( "jsMVC-controller" ) ) {
                                                        jQuery( sv.containerSelector ).addClass( "jsMVC-controller-" + uuid );
                                                }
                                        }
                                }
                        }
                        // Create the jQuery wrapper, find only elements on this view and not
                        // on subviews that have their own controllers.
                        jQueryWrapper = jsMVC.render.jQueryWrapper( viewContainerSelector, ".jsMVC-controller" );
                        controllerFunction.apply( controllerFunction, [ undefined, undefined, undefined, jQueryWrapper, jQueryWrapper, controllerParams ] );
                }
                // View is ready to be used.
// jQuery( viewContainerSelector ).unwrap();
// jQuery( viewContainerSelector ).removeClass( "invisible" );
                jQuery( viewContainerSelector ).children().last().remove();
                // Resolve the returned deferred when all is done.
                deferred.resolve();
        });

        // Return the promise only.
        return deferred.promise();
};

// Parses the container looking for nodes having a class name jsMVC-view to 
// render. For each node found it looks inside its tag attributes for the view 
// and controller name and parameters to load.
// Returns an array with the properties of each view.
jsMVC.render.html = function ( viewContainerSelector ) {
        // The deferred to return.
        var deferred = jQuery.Deferred();
        // The views deferred processors.
        var deferredArray = [];
        // The views info.
        var viewsArray = [];
        // For each view container.
        jQuery( viewContainerSelector ).find( ".jsMVC-view" ).each( function () {
                var innerViewContainerSelector = jQuery( this );
                var viewNameToInclude = innerViewContainerSelector.attr( "data-jsMVC-view" );
                if ( viewNameToInclude !== undefined && typeof viewNameToInclude === "string" && viewNameToInclude !== "" ) {
                        var controllerNameToUse = innerViewContainerSelector.attr( "data-jsMVC-controller" );
                        // TODO: Where can I put this inside the HTML ??
                        var controllerParamsText = innerViewContainerSelector.attr( "data-jsMVC-controller-parameters" );
                        var controllerParamsToUse = undefined;
                        try {
                                controllerParamsToUse = JSON.parse( controllerParamsText );
                        } catch ( err ) {

                        }
                        // Calls render when all the parameters are known.
                        var viewDeferred = jsMVC.render( innerViewContainerSelector, viewNameToInclude, controllerNameToUse, controllerParamsToUse );
                        // Add the view info to the accumulators.
                        deferredArray.push( viewDeferred );
                        viewsArray.push({
                                "containerSelector": innerViewContainerSelector,
                                "viewName": viewNameToInclude,
                                "controllerName": controllerNameToUse,
                                "controllerParams": controllerParamsToUse
                        });
                }
        });
        // Wait for all views to finish rendering.
        // Use JavaScript's apply to call "when" with al the deferreds as parameters.
        jQuery.when.apply( jQuery, deferredArray ).done( function () {
                // Resolve with an array of {selector, name} objects.
                deferred.resolve( viewsArray );
        });
        // Return the promise only.
        return deferred.promise();
};

// Rename the "src" attribute to "data-jsMVC-scr" of each image on the provided string.
jsMVC.render.alterImages = function ( viewString ) {
        // Put the view inside a div so we can alter this dom fragment without including it on the document.
        // Without this it would be impossible to alter the DOM in the string.
        // If this view is inserted to the dom the images will start loading.
        var view = jQuery( "<div>" + viewString + "</div>", document );
        view.find( 'img' ).each(function () {
                var img = jQuery( this );
                // Do not alter the images that have as source the spinner in
                // its cannonical path, put by a previous call to this method.
                var spinner= "/_/s/html/images/jsMVC/spinner.gif";
                if ( img.attr( "src" ) !== spinner ) {
                        img.attr( "data-jsMVC-src", img.attr( "src" ) );
                        // Put the temporary spinner.
                        img.attr( "src", spinner );
                }
        });
        // Return the string content of the div.
        return view.html();
};

jsMVC.render.loadImages = function ( viewContainerSelector ) {
        // Now load every image asynchronously and simultaneosly.
        jQuery( viewContainerSelector ).find( 'img' ).each( function () {
                var imgElement = jQuery( this );
                // Only alter the images that were previously edited.
                var src = imgElement.attr( "data-jsMVC-src" );
                if ( src ) {
                        jsMVC.image.load( src )
                        .done( function ( uri ) {
                                imgElement.attr( "src", uri );
                        })
                        .fail(function ( uri ) {
                        });
                }
        });
};

// Replace the params to the placeholder on the viewstring (Elements with class
// jsMVC-view-param and named data-jsMVC-view-param).
jsMVC.render.viewParams = function ( viewString, viewParamsValues ) {
        // Put the view inside a div so we can alter this dom fragment without including it on the document.
        // Without this it would be impossible to alter the DOM in the string.
        // If this view is inserted to the dom the images will start loading.
        var view = jQuery( "<div>" + viewString + "</div>", document ); 
        view.find( ".jsMVC-view-param" ).each( function () {
                var paramElem = jQuery( this );
                var paramName = paramElem.attr( "data-jsMVC-view-param" );
                if ( paramName !== undefined && paramName !== null && typeof paramName === 'string' && paramName !== "" ) {
                        var paramValue = viewParamsValues[ paramName ];
                        // Has this param a value ?
                        if ( paramValue !== undefined ) {
                                paramElem.html( paramValue );
                        // If not the param of a subview (inmediate child of a "jsMVC-view") ?
                        } else if ( ! paramElem.parent().hasClass( "jsMVC-view" ) ) {
                                paramElem.remove();
                        }
                }
        });
        // Return the string content of the div.
        return view.html();
};

// I NEED THIS: https://github.com/tvcutsem/harmony-reflect

jsMVC.render.jQueryWrapper = function ( context, notChildOf ) {
        var ans = function ( selector ) {
                // jQuery without parameters, return the view container selector.
                if ( selector === undefined || selector === null || selector === "" || selector === "*" ) {
                        return jQuery( context );
                }
                // With find() I search through all the children of context.
                // Doing jQuery( selector, context ) the context element is included.
                return jQuery( context ).find( selector ).filter( function () {
                        // Filter all the found element inside the context to the ones
                        // not having a parent (that is not the container) that has a 
                        // controller.
                        return jQuery( this ).parentsUntil( context, notChildOf ).length == 0;
                });
        };
        ans.ajax = jQuery.ajax;
        ans.Callbacks = jQuery.Callbacks;
        ans.Deferred = jQuery.Deferred;
        ans.isArray = jQuery.isArray;
        ans.isEmptyObject = jQuery.isEmptyObject;
        ans.isFunction = jQuery.isFunction;
        ans.isNumeric = jQuery.isNumeric;
        ans.isPlainObject = jQuery.isPlainObject;
        // The equivalent of doing jQuery( "<div></div>" )
        ans.htmlString = function ( string ) {
                // Create a jQuery object with the parsed elements.
                return jQuery(
                        // Returns an array.
                        jQuery.parseHTML( string, document, false )
                );
        };
        ans.trim = jQuery.trim;
        ans.when = jQuery.when;
        return ans;
};

jsMVC.render.findBootstrapEnvironment = function () {
        // Available responsive classes.
        // 'xl' only available on Bootstrap 4.
        var classes = ['xs', 'sm', 'md', 'lg', 'xl'];
        // Add an element to test it.
        var element = jQuery( '<div>' );
        element.appendTo( jQuery('body') );
        // Query if hidden for all the classes.
        for ( var i = 0; i >= classes.length; i++ ) {
                var suffix = classes[i];
                element.addClass( 'hidden-' + suffix );
                if ( element.is( ':hidden' ) ) {
                    element.remove();
                    return suffix;
                }
        }
};
