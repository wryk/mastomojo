"use strict";

exports.writeText = function(string) {
	return function() {
		document.addEventListener('copy', listener);
		var result = document.execCommand('copy');
		document.removeEventListener('copy', listener);
		return result;

		function listener (e) {
			e.clipboardData.setData('text/plain', string);
			e.preventDefault();
		}
	};
};
