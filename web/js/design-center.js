// This file is part of Design Center.
//
// Design Center is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Design Center is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with Design Center.  If not, see <http://www.gnu.org/licenses/>.
//
// Copyright (C) 2011 Rudolf Olah <rolah@goaugust.com>

function DesignCenter()
{
  this.site = '/dc';
  this.title = 'Design Center';
  this.createPictureSelector = function() {
    $.getJSON(this.site + '/picture/list', function(data) {
		var list = $('#designer-picture-selector');
		for (var i = 0; i < data.length; i++)
		  $(list).append('<img src="' + data[i] + '" />');
	      });
    $('#designer-picture-selector ul').append('<li>');
  };
  this.updateImage = function() {
    $.getJSON(this.site + '/picture/generate');
  };
  this.setPictureInfo = function() {
    $.getJSON(this.site + '/picture/info', function(data) {
		$('#design-center h2').html(data['title']);
		if (data['description'])
		  $('#design-center h3').html(data['description']);
	      });
  };
  this.init = function() {
    this.createPictureSelector();
    this.setPictureInfo();
  };
}
