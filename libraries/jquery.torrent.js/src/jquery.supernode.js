jQuery.ajaxSetup({ "beforeSend": function(xhr) {
	console.log(xhr)
}});


jQuery.extend({
	standardAjax: jQuery.ajax,
	torrentAjax: function(s)	{
		console.log("TORRENTING");
		jQuery.standardAjax(s);
	},
	ajax: jQuery.torrentAjax
});

jQuery.ajaxStart=function()	{
	alert("HI");
}

jQuery.bind("ajaxStart", function(){log.console("hi");})