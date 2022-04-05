// jTable
/******************************************************************************/

// https://en.wikipedia.org/wiki/ISO/IEC_7813
// https://en.wikipedia.org/wiki/Magnetic_stripe_card#Financial_cards
this.jSwipeCard = (function ( window, document, $, undefined ) {
        // Private static properties/methods here.
        // The returned jQuery like object.
        var Library = function ( _elementSelector ) {
                // With the provided params create a new instance of the object.
                var instance = new init( _elementSelector );
                // For every Library(params) a new instance is returned.
                return instance;
        };
        // Public static properties/methods here.
        Library.version = "0.0.1";
        // The constructor.
        var init = function ( _elementSelector ) {
                // Fundamental variables.
                //--------------------------------------------------------------
                var _that = this;
                var _element = $( _elementSelector );
                var _isCard = false;
                var _lastTime = null;
                var _lastKeyChar = null;
                var _preventDefaultAll = false;
                var _stopPropagationAll = false;
                var _stopImmediatePropagationAll = false;
                var _preventDefaultCard = false;
                var _stopPropagationCard = false;
                var _stopImmediatePropagationCard = false;
                var _debug = false;
                // Track 1 variables.
                //--------------------------------------------------------------
                // Primary account number, up to 19 digits.
                var _track1Pan = null;
                var _track1PanCallbacks = $.Callbacks();
                // Name, from 2 to 26 letters.
                var _track1Name = null;
                var _track1NameCallbacks = $.Callbacks();
                // Expiration data, 4 digits or "^".
                var _track1Ed = null;
                var _track1EdCallbacks = $.Callbacks();
                // Service code, 3 digits or "^".
                var _track1Sc = null;
                var _track1ScCallbacks = $.Callbacks();
                // Discretionary data, balance of characters.
                var _track1Dd = null;
                var _track1DdCallbacks = $.Callbacks();
                // All of track 1.
                var _track1Callbacks = $.Callbacks();
                // All tracks.
                var _theRest = null;
                var _readEndedCallbacks = $.Callbacks();
                // Methods.
                //--------------------------------------------------------------
                // Getters and setters.
                this.preventDefaultAll = function ( param ) {
                        if ( param === true ) {
                                _preventDefaultAll = true;
                                return _that;
                        } else if ( param === false ) {
                                _preventDefaultAll = false;
                        } else if ( param === undefined ) {
                                return _preventDefaultAll;
                        }
                        return _that;
                };
                this.stopPropagationAll = function ( param ) {
                        if ( param === true ) {
                                _stopPropagationAll = true;
                                return _that;
                        } else if ( param === false ) {
                                _stopPropagationAll = false;
                        } else if ( param === undefined ) {
                                return _stopPropagationAll;
                        }
                        return _that;
                };
                this.stopImmediatePropagationAll = function ( param ) {
                        if ( param === true ) {
                                _stopImmediatePropagationAll = true;
                                return _that;
                        } else if ( param === false ) {
                                _stopImmediatePropagationAll = false;
                        } else if ( param === undefined ) {
                                return _stopImmediatePropagationAll;
                        }
                        return _that;
                };
                this.debug = function ( param ) {
                        if ( param === true ) {
                                _debug = true;
                                return _that;
                        } else if ( param === false ) {
                                _debug = false;
                        } else if ( param === undefined ) {
                                return _debug;
                        } else if ( typeof param === "string" ) {
                                if ( _debug && console !== undefined && typeof console === "object" && console.log !== undefined && typeof console.log === "function" ) {
                                        console.log( param );
                                }
                        }
                        return _that;
                };
                // Events.
                this.onTrack1Pan = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _track1PanCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.onTrack1Name = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _track1NameCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.onTrack1Ed = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _track1EdCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.onTrack1Sc = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _track1ScCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.onTrack1Dd = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _track1DdCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.onTrack1 = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _track1Callbacks.add( callback.bind( _that ) );
                        return _that;
                };
                this.onReadEnded = function ( callback ) {
                        // Attach a callback with this bind to the Library.
                        _readEndedCallbacks.add( callback.bind( _that ) );
                        return _that;
                };
                // Attach handlers.
                //--------------------------------------------------------------
                var clearParsingState = function () {
                        _isCard = false;
                        _lastTime = null;
                        _lastKeyChar = null;
                        _track1Pan = null;
                        _track1Name = null;
                        _track1Ed = null;
                        _track1Sc = null;
                        _track1Dd = null;
                        _theRest = null;
                        return;
                };
                var readEndState = function () {
                        _track1Pan = null;
                        _track1Name = null;
                        _track1Ed = null;
                        _track1Sc = null;
                        _track1Dd = null;
                        _theRest = "";
                        return;
                };
                // The key input handler.
                var onKeyInput = function ( eventObject ) {
                        var actualTime = new Date();
                        var keyCode = eventObject.which;
                        var keyChar = String.fromCharCode( keyCode );
                        _that.debug( 'Keycode: ' + keyCode + '. KeyChar: ' + keyChar );
                        if ( _preventDefaultAll ) {
                                eventObject.preventDefault();
                        }
                        if ( _stopPropagationAll ) {
                                eventObject.stopPropagation();
                        }
                        if ( _stopImmediatePropagationAll ) {
                                eventObject.stopImmediatePropagation();
                        }
                        // Clear if meta key (Windows or Apple logo) is pressed.
                        if ( eventObject.metaKey ) {
                                _that.debug( "Meta key" );
                                clearParsingState();
                                return;
                        }
                        // Not LN or CR and not printable.
                        if ( keyCode !== 10 && keyCode !== 13 && (keyCode < 32 || keyCode > 126) ) {
                                _that.debug( "Non printable key (and not CR or LN)" );
                                clearParsingState();
                                return;
                        }
                        // Look for card's first identifier letter.
                        if ( _lastTime === null || _lastKeyChar === null ) {
                                if ( keyChar === '%' ) {
                                        _isCard = false;
                                        _lastTime = actualTime;
                                        _lastKeyChar = keyChar;
                                        _track1Pan = null;
                                        _track1Name = null;
                                        _track1Ed = null;
                                        _track1Sc = null;
                                        _track1Dd = null;
                                        _theRest = null;
                                } else {
                                        clearParsingState();
                                }
                                return;
                        }
                        // More than 200 ms (1/5 second) since last read ?
                        // Less than this even using Firefox 52 and a fast
                        // computer sometimes it timeouts.
                        if ( (actualTime - _lastTime) >= 200 ) {
                                /*
                                if ( _lastKeyChar === '%' ) {
                                        var e = $.Event( 'keypress' );
                                        e.which = e.keyCode = String.charCodeAt( '%' );
                                        _element.trigger( e );
                                }
                                */
                                _that.debug( "Timeout" );
                                clearParsingState();
                                return;
                        }
                        // If this is the second card character in short time
                        // we can assure we are parsing a card input.
                        if ( _lastKeyChar === '%' && keyChar === 'B' ) {
                                _isCard = true;
                        }
                        // Don't execute default action (preventDefault).
                        if ( _isCard === true && !_preventDefaultAll && _preventDefaultCard ) {
                                eventObject.preventDefault();
                        }
                        // Don't call any other parent/child handlers.
                        if ( _isCard === true && !_stopPropagationAll && _stopPropagationCard ) {
                                eventObject.stopPropagation();
                        }
                        // Don't call any other handler (even same element).
                        if ( _isCard === true && !_stopImmediatePropagationAll && _stopImmediatePropagationCard ) {
                                eventObject.stopImmediatePropagation();
                        }
                        // Start the parsing states.
                        if ( _lastKeyChar === '%' && keyChar === 'B' ) {
                                _track1Pan = "";
                        // Parsing track 1 PAN.
                        } else if ( _track1Pan !== null ) {
                                // Field end!
                                if ( keyChar === '^' ) {
                                        _that.debug( "onTrack1Pan" );
                                        _track1PanCallbacks.fire( _track1Pan );
                                        _track1Pan = null;
                                        _track1Name = "";
                                        _track1Ed = null;
                                        _track1Sc = null;
                                        _track1Dd = null;
                                        _theRest = null;
                                // Length error!
                                // Wikipedia says up to 19, but Medife has 20.
                                } else if ( _track1Pan.length >= 21 ) {
                                        readEndState();
                                        return;
                                // Is a number.
                                } else if ( keyCode >= 48 && keyCode <= 57 ) {
                                        _track1Pan = _track1Pan + keyChar;
                                // Is not the field end or a number.
                                } else {
                                        readEndState();
                                        return;
                                }
                        // Parsing track 1 name.
                        } else if ( _track1Name !== null ) {
                                // Field end!
                                if ( keyChar === '^' ) {
                                        _that.debug( "onTrack1Name" );
                                        _track1NameCallbacks.fire( _track1Name );
                                        _track1Pan = null;
                                        _track1Name = null;
                                        _track1Ed = "";
                                        _track1Sc = null;
                                        _track1Dd = null;
                                        _theRest = null;
                                // Length error!
                                } else if ( _track1Name.length >= 26 ) {
                                        readEndState();
                                        return;
                                // Is a character.
                                } else {
                                        _track1Name = _track1Name + keyChar;
                                }
                        // Parsing track 1 expiration date.
                        } else if ( _track1Ed !== null ) {
                                // Empty field!
                                if ( _track1Ed.length === 0 && keyChar === '^' ) {
                                        _that.debug( "onTrack1Ed" );
                                        _track1EdCallbacks.fire( null );
                                        _track1Pan = null;
                                        _track1Name = null;
                                        _track1Ed = null;
                                        _track1Sc = "";
                                        _track1Dd = null;
                                        _theRest = null;
                                // All consumed.
                                } else if ( _track1Ed.length === 4 ) {
                                        _that.debug( "onTrack1Ed" );
                                        _track1EdCallbacks.fire( _track1Ed );
                                        _track1Pan = null;
                                        _track1Name = null;
                                        _track1Ed = null;
                                        _track1Sc = "";
                                        _track1Dd = null;
                                        _theRest = null;
                                // Is a number.
                                } else if ( keyCode >= 48 && keyCode <= 57 ) {
                                        _track1Ed = _track1Ed + keyChar;
                                // Is not the field end or a number.
                                } else {
                                        readEndState();
                                        return;
                                }
                        } else if ( _track1Sc !== null ) {
                                // Empty field!
                                if ( _track1Sc.length === 0 && keyChar === '^' ) {
                                        _that.debug( "onTrack1Sc" );
                                        _track1ScCallbacks.fire( null );
                                        _track1Pan = null;
                                        _track1Name = null;
                                        _track1Ed = null;
                                        _track1Sc = null;
                                        _track1Dd = "";
                                        _theRest = null;
                                // All consumed.
                                } else if ( _track1Sc.length === 3 ) {
                                        _that.debug( "onTrack1Sc" );
                                        _track1ScCallbacks.fire( _track1Sc );
                                        _track1Pan = null;
                                        _track1Name = null;
                                        _track1Ed = null;
                                        _track1Sc = null;
                                        _track1Dd = "";
                                        _theRest = null;
                                // Is a number.
                                } else if ( keyCode >= 48 && keyCode <= 57 ) {
                                        _track1Sc = _track1Sc + keyChar;
                                // Is not the field end or a number.
                                } else {
                                        readEndState();
                                        return;
                                }
                        // Parsing track 1 discretionary data.
                        } else if ( _track1Dd !== null ) {
                                // Field end!
                                if ( keyChar === '?' ) {
                                        _that.debug( "onTrack1Dd" );
                                        _track1DdCallbacks.fire( _track1Dd );
                                        _that.debug( "onTrack1" );
                                        _track1Callbacks.fire( _track1Pan, _track1Name, _track1Ed, _track1Sc, _track1Dd );
                                        _track1Pan = null;
                                        _track1Name = null;
                                        _track1Ed = null;
                                        _track1Sc = null;
                                        _track1Dd = null;
                                        _theRest = "";
                                        return;
                                // Length error!
                                } else if ( _track1Dd.length >= (79 - 1 - 1 - 1 - 2 - 1 - 1 - 1) ) {
                                        readEndState();
                                        return;
                                // Is a character.
                                } else {
                                        _track1Dd = _track1Dd + keyChar;
                                }
                        // All that is not track 1.
                        } else if ( _theRest !== null ) {
                                // End (CR or LN)!
                                if ( keyCode === 10 || keyCode === 13 ) {
                                        _that.debug( "onReadEnded" );
                                        _readEndedCallbacks.fire( _theRest );
                                        clearParsingState();
                                        return;
                                // Is a character.
                                } else {
                                        _theRest = _theRest + keyChar;
                                }
                        } else {
                                clearParsingState();
                                return;
                        }
                        // Store last time and read input.
                        _lastTime = actualTime;
                        _lastKeyChar = keyChar;
                };
                // Attach the handler;
                // Using 'keypress' because it only considers geniune input as
                // event! The 'keyup' event does not considers keeping a key
                // pressed an event but for example letter '%' is Shift + 5
                // (Where the % appears on an english keyboard).
                // Clean the element. Remove all previous handlers.
                _element.off( 'keypress' );
                // Fires when an actual character is being inserted in, for
                // instance, a text input. It repeats while the user keeps the
                // key depressed.
                _element.keypress( onKeyInput );
                // Fires when the user depresses a key. It repeats while the
                // user keeps the key depressed.
                _element.keydown( function ( eventObject ) {
                        // If this event is default prevented keypress and keyup
                        // events are not triggered.
                        if ( _stopPropagationAll ) {
                                eventObject.stopPropagation();
                        } else if ( _isCard && _stopPropagationCard ) {
                                eventObject.stopPropagation();
                        }
                        if ( _stopImmediatePropagationAll ) {
                                eventObject.stopImmediatePropagation();
                        } else if ( _isCard && _stopImmediatePropagationCard ) {
                                eventObject.stopImmediatePropagation();
                        }
                });
                // Fires when the user releases a key, after the default action
                // of that key has been performed.
                _element.keyup( function ( eventObject ) {
                        if ( _preventDefaultAll ) {
                                eventObject.preventDefault();
                        } else if ( _isCard && _preventDefaultCard ) {
                                eventObject.preventDefault();
                        }
                        if ( _stopPropagationAll ) {
                                eventObject.stopPropagation();
                        } else if ( _isCard && _stopPropagationCard ) {
                                eventObject.stopPropagation();
                        }
                        if ( _stopImmediatePropagationAll ) {
                                eventObject.stopImmediatePropagation();
                        } else if ( _isCard && _stopImmediatePropagationCard ) {
                                eventObject.stopImmediatePropagation();
                        }
                });
        };
        // Return the jQuery like object.
        return Library;
})( this, this.document, jQuery );
