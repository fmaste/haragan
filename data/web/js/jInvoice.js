// jInvoice
/******************************************************************************/

this.jInvoice = (function ( window, document, $, undefined ) {
	var Library = function ( tableSelector ) {
		var instance = new init( tableSelector );
		return instance;
	};
	Library.version = "0.0.1";
	// Constructor.
	//---------------------------------------------------------------------------
	var init = function ( tableSelector ) {
		var _that = this;
		var _jTable = jTable( tableSelector );
		var _items = [];
		var _grossReceiptsTax = 0;
		var _taxes = {};
		var _onTotalsUpdatedCallbacks = $.Callbacks();
		// Columns.
		//----------------------------------------------------------------------
		// Item order.
		var _columnOrder = {name: "#", hidden: {xs: true, sm: true, print: true}, getter: "order"};
		_jTable.column( "order", _columnOrder );
		// The shown ID.
		var _columnId = {name: "ID", hidden: {}, getter: "itemId"};
		_jTable.column( "id", _columnId );
		// The shown name.
		var _columnName = {name: "Nombre", hidden: {}, getter: "itemName"};
		_jTable.column( "name", _columnName );
		// Quantity input.
		var _columnQuantity = {name: "Cantidad", hidden: {}, getter: function ( cell, order ) {
			var inputGroup = $( "<div></div>" )
			.addClass( 'input-group' )
			.addClass( 'hidden-print' );
			var input = $( "<input type='number' required placeholder='...'>" )
			.addClass( 'form-control' );
			var popoverRightAddOn = $( "<a tabindex='0'></a>" )
			.addClass( ' input-group-addon' )
			.addClass( 'hidden-xs' )
			.addClass( 'hidden-sm' );
			var glyphicon = $( "<span></span>" )
			.addClass( 'glyphicon glyphicon-comment' );
			var printSpan = $( "<span></span>" )
			.addClass( 'visible-print-block' );
			_items[ order ].quantity.cell = cell;
			_items[ order ].quantity.input = input;
			_items[ order ].quantity.popover = popoverRightAddOn;
			_items[ order ].quantity.print = printSpan;
			cell.append( 
				inputGroup
					.append( input )
					.append( 
						popoverRightAddOn
							.append( glyphicon ) 
					) 
			);
			cell.append( printSpan );
		}};
		_jTable.column( "quantity", _columnQuantity );
		// Price input.
		var _columnPrice = {name: "Precio", hidden: {}, getter: function ( cell, order ) {
			var inputGroup = $( "<div></div>" )
			.addClass( 'input-group' )
			.addClass( 'hidden-print' );
			var input = $( "<input type='number' required placeholder='...'>" )
			.addClass( 'form-control' );
			var popoverRightAddOn = $( "<a tabindex='0'></a>" )
			.addClass( ' input-group-addon' )
			.addClass( 'hidden-xs' )
			.addClass( 'hidden-sm' );
			var glyphicon = $( "<span></span>" )
			.addClass( 'glyphicon glyphicon-comment' );
			var printSpan = $( "<span></span>" )
			.addClass( 'visible-print-block' );
			_items[ order ].price.cell = cell;
			_items[ order ].price.input = input;
			_items[ order ].price.popover = popoverRightAddOn;
			_items[ order ].price.print = printSpan;
			cell.append( 
				inputGroup
					.append( input )
					.append( 
						popoverRightAddOn
							.append( glyphicon ) 
					) 
			);
			cell.append( printSpan );
		}};
		_jTable.column( "price", _columnPrice );
		// Calculated subtotal.
		var _columnSubtotal = {name: "Importe", hidden: {}, getter: function ( cell, order ) {
			var inputGroup = $( "<div></div>" )
			.addClass( 'input-group' )
			.addClass( 'hidden-print' );
			var input = $( "<input disabled></input>" )
			.addClass( 'form-control' );
			var usdLeftAddOn = $( "<span></span>" )
			.addClass( 'input-group-addon' )
			.addClass( 'hidden-xs' )
			.addClass( 'hidden-sm' )
			.addClass( 'hidden-md' );
			var glyphicon = $( "<span></span>" )
			.addClass( 'glyphicon glyphicon-usd' );
			var printSpan = $( "<span></span>" )
			.addClass( 'visible-print-block' );
			_items[ order ].subtotal.cell = cell;
			_items[ order ].subtotal.input = input;
			_items[ order ].subtotal.print = printSpan;
			cell.append( 
				inputGroup
					.append( 
						usdLeftAddOn.append( glyphicon )
					)
					.append( input ) 
			);
			cell.append( printSpan );
		}};
		_jTable.column( "subtotal", _columnSubtotal );
		// Excises input.
		var _columnExcises = {name: "Excentos", hidden: {}, getter: function ( cell, order ) {
			var inputGroup = $( "<div></div>" )
			.addClass( 'input-group' )
			.addClass( 'hidden-print' );
			var input = $( "<input type='number' required placeholder='...'>" )
			.addClass( 'form-control' );
			var usdLeftAddOn = $( "<span></span>" )
			.addClass( 'input-group-addon' )
			.addClass( 'hidden-xs' )
			.addClass( 'hidden-sm' )
			.addClass( 'hidden-md' );
			var glyphicon = $( "<span></span>" )
			.addClass( 'glyphicon glyphicon-usd' );
			var printSpan = $( "<span></span>" )
			.addClass( 'visible-print-block' );
			_items[ order ].excises.cell = cell;
			_items[ order ].excises.input = input;
			_items[ order ].excises.print = printSpan;
			cell.append( 
				inputGroup
					.append( 
						usdLeftAddOn.append( glyphicon )
					)
					.append( input ) 
			);
			cell.append( printSpan );
		}};
		_jTable.column( "excises", _columnExcises );
		// Calculated net total.
		var _columnNetTotal = {name: "Gravado", hidden: {xs: true, sm: true, print: true}, getter: function ( cell, order ) {
			var inputGroup = $( "<div></div>" )
			.addClass( 'input-group' )
			.addClass( 'hidden-print' );
			var input = $( "<input disabled></input>" )
			.addClass( 'form-control' );
			var usdLeftAddOn = $( "<span></span>" )
			.addClass( 'input-group-addon' )
			.addClass( 'hidden-xs' )
			.addClass( 'hidden-sm' )
			.addClass( 'hidden-md' );
			var glyphicon = $( "<span></span>" )
			.addClass( 'glyphicon glyphicon-usd' );
			var printSpan = $( "<span></span>" )
			.addClass( 'visible-print-block' );
			_items[ order ].net.cell = cell;
			_items[ order ].net.input = input;
			_items[ order ].net.print = printSpan;
			cell.append( 
				inputGroup
					.append( 
						usdLeftAddOn.append( glyphicon )
					)
					.append( input ) 
			);
			cell.append( printSpan );
		}};
		_jTable.column( "net-total", _columnNetTotal );
		// VAT percentage input.
		var _columnVatPercentage = {name: "IVA %", hidden: {}, getter: function ( cell, order ) {
			var inputGroup = $( "<div></div>" )
			.addClass( 'input-group' )
			.addClass( 'hidden-print' );
			var input = $( "<input type='number' required placeholder='...'>" )
			.addClass( 'form-control' );
			var percentageRightAddOn = $( "<span>%</span>" )
			.addClass( 'input-group-addon' )
			.addClass( 'hidden-xs' )
			.addClass( 'hidden-sm' )
			.addClass( 'hidden-md' );
			var printSpan = $( "<span></span>" )
			.addClass( 'visible-print-block' );
			_items[ order ].vatPercentage.cell = cell;
			_items[ order ].vatPercentage.input = input;
			_items[ order ].vatPercentage.print = printSpan;
			cell.append( 
				inputGroup
					.append( input )
					.append( percentageRightAddOn ) 
			);
			cell.append( printSpan );
		}};
		_jTable.column( "vat-percentage", _columnVatPercentage );
		// Calculated VAT total.
		var _columnVatTotal = {name: "IVA $", hidden: {xs: true, sm: true, print: true}, getter: function ( cell, order ) {
			var inputGroup = $( "<div></div>" )
			.addClass( 'input-group' )
			.addClass( 'hidden-print' );
			var input = $( "<input disabled></input>" )
			.addClass( 'form-control' );
			var usdLeftAddOn = $( "<span></span>" )
			.addClass( 'input-group-addon' )
			.addClass( 'hidden-xs' )
			.addClass( 'hidden-sm' )
			.addClass( 'hidden-md' );
			var glyphicon = $( "<span></span>" )
			.addClass( 'glyphicon glyphicon-usd' );
			var printSpan = $( "<span></span>" )
			.addClass( 'visible-print-block' );
			_items[ order ].vat.cell = cell;
			_items[ order ].vat.input = input;
			_items[ order ].vat.print = printSpan;
			cell.append( 
				inputGroup
					.append( 
						usdLeftAddOn.append( glyphicon )
					)
					.append( input ) 
			);
			cell.append( printSpan );
		}};
		_jTable.column( "vat-total", _columnVatTotal );
		// Calculated total.
		var _columnTotal = {name: "Subtotal", hidden: {print: true}, getter: function ( cell, order ) {
			var inputGroup = $( "<div></div>" )
			.addClass( 'input-group' )
			.addClass( 'hidden-print' );
			var input = $( "<input disabled></input>" )
			.addClass( 'form-control' );
			var usdLeftAddOn = $( "<span></span>" )
			.addClass( 'input-group-addon' )
			.addClass( 'hidden-xs' )
			.addClass( 'hidden-sm' )
			.addClass( 'hidden-md' );
			var glyphicon = $( "<span></span>" )
			.addClass( 'glyphicon glyphicon-usd' );
			var printSpan = $( "<span></span>" )
			.addClass( 'visible-print-block' );
			_items[ order ].total.cell = cell;
			_items[ order ].total.input = input;
			_items[ order ].total.print = printSpan;
			cell.append( 
				inputGroup
					.append( 
						usdLeftAddOn.append( glyphicon )
					)
					.append( input ) 
			);
			cell.append( printSpan );
		}};
		_jTable.column( "total", _columnTotal );
		// Remove
		var _columnRemove = {name: "X", hidden: {print: true}, getter: function ( cell, order ) {
			var removeButton = $( "<button></button>" )
			.addClass( "btn" )
			.addClass( "btn-xs" )
			.addClass( "btn-danger" );
			var removeIcon = $( "<span></span>" )
			.addClass( "glyphicon")
			.addClass( "glyphicon-remove" );
			cell.append( removeButton.append( removeIcon ) );
		}};
		_jTable.column( "X", _columnRemove );
		// Show the header.
		_jTable.header();
		// Methods.
		//----------------------------------------------------------------------
		this.item = function ( itemId, itemName, defaultVat ) {
			var order = _items.length;
			// Add item to accum.
			_items[ order ] = {
				id: itemId,
				unit: 1,
				quantity: { value: 0.000, cell: null, input: null, print: null, popover: null },
				price: { value: 0.000, cell: null, input: null, print: null, popover: null },
				subtotal: { value: 0.00, cell: null, input: null, print: null },
				discounts: { value: 0.000 },
				excises: { value: 0.000, cell: null, input: null, print: null },
				net: { value: 0.00, cell: null, input: null, print: null },
				vatPercentage: { value: (defaultVat ? defaultVat : 21.000), cell: null, input: null, print: null },
				vat: { value: 0.00, cell: null, input: null, print: null },
				total: { value: 0.00, cell: null, input: null, print: null }
			};
			// Add item to table.
			_jTable.row( order, {
				order: ( order + 1 ), 
				itemId: itemId, 
				itemName: itemName
			} );
			// Item properties are filled when the jTable row is created.
			var item = _items[ order ];
			var quantity = item.quantity;
			var price = item.price;
			var excises = item.excises;
			var vatPercentage = item.vatPercentage;
			// Default input values.
			quantity.input.val( quantity.value );
			quantity.print.text( quantity.value );
			price.input.val( price.value );
			price.print.text( price.value );
			excises.input.val( excises.value );
			excises.print.text( excises.value );
			vatPercentage.input.val( vatPercentage.value );
			vatPercentage.print.text( vatPercentage.value );
			// Show the totals.
			_that.updateItemTotals( order );
			// Add item quantity change handler.
			var onRowQuantityChanged = function ( order, focus ) {
				return function ( event ) {
					var target = $( event.target );
					_that.quantity( order, target.val(), focus );
				};
			} ( order, price.input );
			quantity.input.on( 'change', onRowQuantityChanged );
			// Add item price change handler.
			var onRowPriceChanged = function ( order, focus ) {
				return function ( event ) {
					var target = $( event.target );
					_that.price( order, target.val(), focus );
				};
			} ( order, excises.input );
			price.input.on( 'change', onRowPriceChanged );
			// Add item excises change handler.
			var onRowExicesChanged = function ( order, focus ) {
				return function ( event ) {
					var target = $( event.target );
					_that.excises( order, target.val(), focus );
				};
			} ( order, vatPercentage.input );
			excises.input.on( 'change', onRowExicesChanged );
			// Add item vat-percentage change handler.
			var onRowVatPercentageChanged = function ( order ) {
				return function ( event ) {
					var target = $( event.target );
					var nextItem = _items[ order + 1 ];
					var focus = undefined;
					if ( nextItem !== undefined ) {
						focus = nextItem.quantity.input;
					}
					_that.vatPercentage( order, target.val(), focus );
				};
			} ( order );
			vatPercentage.input.on( 'change', onRowVatPercentageChanged );
			// Quantity popover.
			var quantityPopover = quantity.cell.find( 'a' );
			var quantityPopoverForm = $( "<form class='form-inline'></form>" );
			var quantityPopoverMayorFormGroup = $( "<div class='form-group'></form>" );
			var quantityPopoverMayorInputGroup = $( "<div class='input-group'></div>" );
			var quantityPopoverMayorInput = $( "<input type='number' class='form-control' placeholder='...''>" );
			var quantityPopoverMayorInputGroupAddOnRight = $( "<div class='input-group-addon'>Pack(s)</div>" );
			var quantityPopoverMinorFormGroup = $( "<div class='form-group'></form>" );
			var quantityPopoverMinorInputGroup = $( "<div class='input-group'></div>" );
			var quantityPopoverMinorInput = $( "<input type='number' class='form-control' placeholder='...''>" );
			var quantityPopoverMinorInputGroupAddOnLeft = $( "<div class='input-group-addon'>De</div>" );
			var quantityPopoverSubmit = $( "<button class='btn btn-default'><span class='glyphicon glyphicon-ok' aria-hidden='true'></span></button>" );
			quantityPopoverSubmit.on( 'click', function ( order, quantityPopover, quantityPopoverMayorFormGroup, quantityPopoverMayorInput, quantityPopoverMinorFormGroup, quantityPopoverMinorInput, priceInput ) {
				return function ( event ) {
					event.preventDefault();
					var errors = false;
					var mayorVal = quantityPopoverMayorInput.val().trim();
					var mayorNum = Number( mayorVal );
					var minorVal = quantityPopoverMinorInput.val().trim();
					var minorNum = Number( minorVal );
					if ( mayorVal === "" || isNaN( mayorNum ) || !isFinite( mayorNum ) || mayorNum <= 0 ) {
						errors = true;
						quantityPopoverMayorFormGroup.addClass( 'has-error' );
					} else {
						quantityPopoverMayorFormGroup.removeClass( 'has-error' );
					}
					if ( minorVal === "" || isNaN( minorNum ) || !isFinite( minorNum ) || minorNum <= 0 ) {
						errors = true;
						quantityPopoverMinorFormGroup.addClass( 'has-error' );
					} else {
						quantityPopoverMinorFormGroup.removeClass( 'has-error' );
					}
					if ( errors === false ) {
						_that.quantity( order, mayorNum * minorNum );
						quantityPopover.popover( 'hide' );
						priceInput.focus();
					}
				};
			}( order, quantityPopover, quantityPopoverMayorFormGroup, quantityPopoverMayorInput, quantityPopoverMinorFormGroup, quantityPopoverMinorInput, price.input ));
			quantityPopover.popover({
				container: 'body',
				content: 
					quantityPopoverForm
					.append( 
						quantityPopoverMayorFormGroup.append( 
							quantityPopoverMayorInputGroup.append( quantityPopoverMayorInput ).append( quantityPopoverMayorInputGroupAddOnRight ) 
						) 
					)
					.append( 
						quantityPopoverMinorFormGroup.append( 
							quantityPopoverMinorInputGroup.append( quantityPopoverMinorInputGroupAddOnLeft ).append( quantityPopoverMinorInput ) 
						) 
					)
					.append( quantityPopoverSubmit ),
				html: true,
				placement: 'rigth',
				template: '<div class="popover" role="tooltip"><div class="arrow"></div><h3 class="popover-title"></h3><div class="popover-content"></div></div>',
				title: "Calcular cantidad",
				trigger: 'click'
			});
			quantityPopover.on( 'shown.bs.popover', function ( quantityPopoverMayorInput ) {
				return function ( event ) {
					quantityPopoverMayorInput.focus();
				};
			}( quantityPopoverMayorInput ));
			// Price popover.
			var pricePopover = price.cell.find( 'a' );
			var pricePopoverForm = $( "<form class='form-inline'></form>" );
			var pricePopoverFormGroup = $( "<div class='form-group'></form>" );
			var pricePopoverInputGroup = $( "<div class='input-group'></div>" );
			var pricePopoverInputGroupAddOnLeft = $( "<div class='input-group-addon'>Importe</div>" );
			var pricePopoverInput = $( "<input type='number' class='form-control' placeholder='...''>" );
			var pricePopoverSubmit = $( "<button class='btn btn-default'><span class='glyphicon glyphicon-ok' aria-hidden='true'></span></button>" );
			pricePopoverSubmit.on( 'click', function ( order, pricePopover, quantityInput, pricePopoverFormGroup, pricePopoverInput, excisesInput ) {
				return function ( event ) {
					event.preventDefault();
					if ( _that.quantity( order ) === 0 ) {
						quantityInput.focus();
					} else {
						var amountVal = pricePopoverInput.val().trim();
						var amountNum = Number( amountVal );
						if ( amountVal !== "" && !isNaN( amountNum ) && isFinite( amountNum ) && amountNum >= 0 ) {
							_that.price( order, amountNum / _that.quantity( order ) );
							pricePopoverFormGroup.removeClass( 'has-error' );
							pricePopover.popover( 'hide' );
							excisesInput.focus();
						} else {
							pricePopoverFormGroup.addClass( 'has-error' );
						}
					}
				};
			}( order, pricePopover, quantity.input, pricePopoverFormGroup, pricePopoverInput, excises.input ));
			pricePopover.popover({
				container: 'body',
				content: 
					pricePopoverForm
					.append( 
						pricePopoverFormGroup.append( 
							pricePopoverInputGroup.append( pricePopoverInputGroupAddOnLeft ).append( pricePopoverInput ) 
						) 
					)
					.append( pricePopoverSubmit ),
				html: true,
				placement: 'rigth',
				template: '<div class="popover" role="tooltip"><div class="arrow"></div><h3 class="popover-title"></h3><div class="popover-content"></div></div>',
				title: "Calcular precio",
				trigger: 'click'
			});
			pricePopover.on( 'shown.bs.popover', function ( pricePopoverInput ) {
				return function ( event ) {
					pricePopoverInput.focus();
				};
			}( pricePopoverInput ));
			return _that;
		};
		this.quantity = function ( order, param, focus ) {
			if ( typeof order === "number" ) {
				var item = _items[ order ];
				if ( item !== undefined ) {
					if ( typeof param === "number" && item.quantity.value !== param ) {
						if ( !isNaN( param) && isFinite( param ) && param > 0 ) {
							_items[ order ].quantity.value = param;
							item.quantity.input.val( Math.round( item.quantity.value * 1000 ) / 1000 );
							item.quantity.print.text( Math.round( item.quantity.value * 1000 ) / 1000 );
							_that.updateItemTotals( order );
							item.quantity.cell.removeClass( "bg-danger" );
							item.quantity.cell.find( "div" ).removeClass( "has-error" );
							if ( focus ) {
								focus.focus();
							}
						} else {
							item.quantity.cell.addClass( "bg-danger" );
							item.quantity.cell.find( "div" ).addClass( "has-error" );
						}
					} else if ( typeof param === "string" ) {
						if ( param.trim() === "" ) {
							return _that.quantity( order, NaN );
						} else {
							return _that.quantity( order, Number( param ), focus );
						}
					} else if ( param === undefined ) {
						return item.quantity.value;
					}
				}
			}
			return _that;
		};
		this.price = function ( order, param, focus ) {
			if ( typeof order === "number" ) {
				var item = _items[ order ];
				if ( item !== undefined ) {
					if ( typeof param === "number" && item.price.value !== param  ) {
						if ( !isNaN( param) && isFinite( param ) && param >= 0  ) {
							_items[ order ].price.value = param;
							item.price.input.val( Math.round( item.price.value * 1000 ) / 1000 );
							item.price.print.text( Math.round( item.price.value * 1000 ) / 1000 );
							_that.updateItemTotals( order );
							item.price.cell.removeClass( "bg-danger" );
							item.price.cell.find( "div" ).removeClass( "has-error" );
							if ( focus ) {
								focus.focus();
							}
						} else {
							item.price.cell.addClass( "bg-danger" );
							item.price.cell.find( "div" ).addClass( "has-error" );
						}
					} else if ( typeof param === "string" ) {
						if ( param.trim() === "" ) {
							return _that.price( order, NaN );
						} else {
							return _that.price( order, Number( param ), focus );
						}
					} else if ( param === undefined ) {
						return item.price.value;
					}
				}
			}
			return _that;
		};
		this.excises = function ( order, param, focus ) {
			if ( typeof order === "number" ) {
				var item = _items[ order ];
				if ( item !== undefined ) {
					if ( typeof param === "number" && item.excises.value !== param ) {
						if ( !isNaN( param) && isFinite( param ) && param >= 0 && param <= ( item.subtotal.value - item.discounts.value ) ) {
							_items[ order ].excises.value = param;
							item.excises.input.val( Math.round( item.excises.value * 1000 ) / 1000 );
							item.excises.print.text( Math.round( item.excises.value * 1000 ) / 1000 );
							_that.updateItemTotals( order );
							item.excises.cell.removeClass( "bg-danger" );
							item.excises.cell.find( "div" ).removeClass( "has-error" );
							if ( focus ) {
								focus.focus();
							}
						} else {
							item.excises.cell.addClass( "bg-danger" );
							item.excises.cell.find( "div" ).addClass( "has-error" );
						}
					} else if ( typeof param === "string" ) {
						if ( param.trim() === "" ) {
							return _that.excises( order, NaN );
						} else {
							return _that.excises( order, Number( param ), focus );
						}
					} else if ( param === undefined ) {
						return item.excises.value;
					}
				}
			}
			return _that;
		};
		this.vatPercentage = function ( order, param, focus ) {
			if ( typeof order === "number" ) {
				var item = _items[ order ];
				if ( item !== undefined ) {
					if ( typeof param === "number" && item.vatPercentage.value !== param ) {
						if ( !isNaN( param) && isFinite( param ) && param >= 0 && param <= 100 ) {
							_items[ order ].vatPercentage.value = param;
							item.vatPercentage.input.val( Math.round( item.vatPercentage.value * 1000 ) / 1000 );
							item.vatPercentage.print.text( Math.round( item.vatPercentage.value * 1000 ) / 1000 );
							_that.updateItemTotals( order );
							item.vatPercentage.cell.removeClass( "bg-danger" );
							item.vatPercentage.cell.find( "div" ).removeClass( "has-error" );
							if ( focus ) {
								focus.focus();
							}
						} else {
							item.vatPercentage.cell.addClass( "bg-danger" );
							item.vatPercentage.cell.find( "div" ).addClass( "has-error" );
						}
					} else if ( typeof param === "string" ) {
						if ( param.trim() === "" ) {
							return _that.vatPercentage( order, NaN );
						} else {
							return _that.vatPercentage( order, Number( param ), focus );
						}
					} else if ( param === undefined ) {
						return item.vatPercentage.value;
					}
				}
			}
			return _that;
		};
		this.grossReceiptsTax = function ( param ) {
			if ( param === undefined ) {
				return _grossReceiptsTax;
			} else if ( typeof param === "number" ) {
				if ( !isNaN( param ) && isFinite( param ) && param >= 0 ) {
					if ( param !== _grossReceiptsTax ) {
						_grossReceiptsTax = param;
						_onTotalsUpdatedCallbacks.fire();
					}
				}
			}
			return _that;
		};
		this.updateItemTotals = function ( order ) {
			if ( typeof order === "number" ) {
				var item = _items[ order ];
				if ( item !== undefined ) {
					item.subtotal.value = item.quantity.value * item.price.value; 
					item.net.value = item.subtotal.value - item.discounts.value - item.excises.value;
					item.vat.value = item.net.value * (item.vatPercentage.value / 100);
					item.total.value = item.net.value + item.vat.value + item.excises.value;
					item.subtotal.input.val( Math.round( item.subtotal.value * 1000 ) / 1000 );
					item.subtotal.print.text( Math.round( item.subtotal.value * 1000 ) / 1000 );
					item.net.input.val( Math.round( item.net.value * 1000 ) / 1000 );
					item.net.print.text( Math.round( item.net.value * 1000 ) / 1000 );
					item.vat.input.val( Math.round( item.vat.value * 1000 ) / 1000 );
					item.vat.print.text( Math.round( item.vat.value * 1000 ) / 1000 );
					item.total.input.val( Math.round( item.total.value * 1000 ) / 1000 );
					item.total.print.text( Math.round( item.total.value * 1000 ) / 1000 );
					_onTotalsUpdatedCallbacks.fire();
				}
				// Hide or show the vat percentage column on the print view.
				var vat = item.vat.value;
				var allTheSameVat = true;
				for ( var i = 0; i < _items.length; i++ ) {
					if ( _items[ order ].vat.value !== vat ) {
						allTheSameVat = false;
						break;
					}
				}
				// TODO: The rows are not reloaded!!
				if ( allTheSameVat && _columnVatPercentage.hidden.print !== true ) {
					//_columnVatPercentage.hidden.print = true;
					//_jTable.column( "vat-percentage", _columnVatPercentage );
				} else if ( !allTheSameVat && _columnVatPercentage.hidden.print === true ) {
					//_columnVatPercentage.hidden.print = false;
					//_jTable.column( "vat-percentage", _columnVatPercentage );
				}
			}
			return _that;
		}
		this.onTotalsUpdated = function ( callback ) {
			// Attach a callback with this bind to the Library.
			_onTotalsUpdatedCallbacks.add( callback.bind( _that ) );
			return _that;
		};
		this.totals = function () {
			var subtotal = 0;
			var discounts = 0;
			var excises = 0;
			var net = 0;
			var vat = 0;
			var taxes = {};
			var total = 0;
			for ( var i = 0; i < _items.length; i++ ) {
				var item = _items[ i ];
				subtotal = subtotal + item.subtotal.value;
				discounts = discounts + item.discounts.value;
				excises = excises + item.excises.value;
				net = net + item.net.value;
				vat = vat + item.vat.value;
				total = total + item.total.value;
			}
			total = total + _grossReceiptsTax;
			return {subtotal: subtotal, discounts: discounts, excises: excises, net: net, vat: vat, grossReceiptsTax: _grossReceiptsTax, total: total};
		};
		this.export = function () {
			var ans = {};
			// Invoice totals.
			var totals = _that.totals();
			ans.invoiceSubtotal = totals.subtotal;
			ans.invoiceDiscounts = totals.discounts;
			ans.invoiceExcises = totals.excises;
			ans.invoiceNet = totals.net;
			ans.invoiceVat = totals.vat;
			ans.invoiceGrossReceiptsTax = totals.grossReceiptsTax;
			ans.invoiceTotal = totals.total;
			// Invoice items.
			ans.items = [];
			for ( var i = 0; i < _items.length; i++ ) {
				var item = _items[ i ];
				var toAdd = {};
				toAdd.invoiceItemId = item.id;
				toAdd.invoiceItemQuantity = item.quantity.value;
				toAdd.invoiceItemPrice = item.price.value;
				toAdd.invoiceItemSubtotal = item.subtotal.value;
				toAdd.invoiceItemExcises = item.excises.value;
				toAdd.invoiceItemDiscounts = item.discounts.value;
				toAdd.invoiceItemVatPercentage = item.vatPercentage.value;
				toAdd.invoiceItemNet = item.net.value;
				toAdd.invoiceItemVat = item.vat.value;
				toAdd.invoiceItemTotal = item.total.value;
				ans.items.push( toAdd );
			}
			return ans;
		};
		this.destroy = function () {
			_onTotalsUpdatedCallbacks.disable();
			_jTable.destroy();
		};
		this.widget = function ( options ) {
			if ( $.isPlainObject( options ) ) {
				// Default values and hanlders.
				if ( options.totalNet ) {
					$( options.totalNet ).text( "0.000" );
				}
				if ( options.totalExcises ) {
					$( options.totalExcises ).text( "0.000" );
				}
				if ( options.totalVat ) {
					$( options.totalVat ).text( "0.000" );
				}
				if ( options.grossReceiptsTax ) {
					$( options.grossReceiptsTax ).val( _that.grossReceiptsTax() );
					// Change handler.
					$( options.grossReceiptsTax ).on( 'change', function ( event ) {
						var target = $( event.target );
						var taxVal = target.val().trim();
						var taxNum = Number( taxVal );
						_that.grossReceiptsTax( taxNum );
					});
				}
				if ( options.totalAll ) {
					$( options.totalAll ).text( "0.000" );
				}
				// On update.
				_that.onTotalsUpdated( function ( recordsTotal ) {
					var totals = _that.totals();
					// Show the total net.
					if ( options.totalNet ) {
						$( options.totalNet ).text( Math.round( totals.net * 1000 ) / 1000 );
					}
					// Show the total excises.
					if ( options.totalExcises ) {
						$( options.totalExcises ).text( Math.round( totals.excises * 1000 ) / 1000 );
					}
					// Show the total vat.
					if ( options.totalVat ) {
						$( options.totalVat ).text( Math.round( totals.vat * 1000 ) / 1000 );
					}
					// Show the gross receipts tax.
					if ( options.grossReceiptsTax ) {
						$( options.grossReceiptsTax ).val( Math.round( totals.grossReceiptsTax * 1000 ) / 1000 );
					}
					// Show the whole total.
					if ( options.totalAll ) {
						$( options.totalAll ).text( Math.round( totals.total * 1000 ) / 1000 );
					}
				});
			}
			return _that;
		};
		// End methods.
		//------------------------------------------------------------------------
	// End constructor.
	//---------------------------------------------------------------------------
	};
	return Library;
})( this, this.document, jQuery );
