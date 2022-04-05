var quirks = function () {

	// Avoid fixed navbar on top of content.
	if ( jQuery( "nav" ).hasClass( "navbar-fixed-top" ) ) { 
		jQuery( function() {
			jQuery( document.body ).css( 'padding-top', jQuery( '.navbar-fixed-top' ).height() + 15 );
			jQuery( window ).resize( function() {
				jQuery( document.body ).css( 'padding-top', jQuery('.navbar-fixed-top').height() + 15 );
			});
		});
	};

	// Add margin top when no navbar.
	if ( jQuery( "nav" ).length === 0 || !jQuery( "nav" ).hasClass( "navbar-fixed-top" ) ) { 
		jQuery( function() {
			jQuery( document.body ).css( 'padding-top', 15 );
			jQuery( window ).resize( function() {
				jQuery( document.body ).css( 'padding-top', 15 );
			});
		});
	};

	// Fix for navbar on iPads (dropdown was not shown.)
	// https://github.com/twbs/bootstrap/issues/3184#issuecomment-6533799
	jQuery( '.navbar-collapse a[data-toggle]' ).click( function() {
		jQuery( '.navbar-collapse' ).css( 'height', '100%' );
	});

	// Hide popovers with trigger 'click' when clicking on another place.
	jQuery( 'body' ).on( 'click', function (e) {
		jQuery( '.popover-container' ).each( function () {
			// The 'is' for buttons that trigger popups.
			// The 'has' for icons within a button that triggers a popup.
			if ( !jQuery( this ).is( e.target ) && jQuery( this ).has( e.target ).length === 0 && jQuery( '.popover' ).has( e.target ).length === 0) {
				jQuery( this ).popover( 'hide' );
			}
		});
	});

	// Hide popovers when ESC is clicked.
	jQuery( document ).keyup( function( e ) {
		if ( e.keyCode == 27 ) { 
			jQuery( '.popover-container' ).each( function () {
				jQuery( this ).popover( 'hide' );
			});
		}
	});

};
