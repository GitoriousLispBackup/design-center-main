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
  this.updateImage = function() {
    $.getJSON(this.site + '/picture/generate');
  };
  this.selectPicture = function(id) {
    $.getJSON(this.site + '/picture/select', {'id':id}, function(data) {
		$('#design-center h2').html(data['title']);
		if (data['description'])
		  $('#design-center h3').html(data['description']);
	      });
  };
  this.addLayer = function(id, name) {
    $('#design-center .picture-layers').append('<div class="layer layer-' + id + '"><span class="layer-name">' + name + '</span><span class="palette"></span></div>');
  };
  this.loadLayers = function() {
    var self = this;
    $.getJSON(self.site + '/picture/layer/list', function(data) {
		$.each(data, self.addLayer);
	      });
  };
  this.addThumbnail = function(data) {

  };
  this.loadThumbnails = function() {
    var self = this;
    $.getJSON(self.site + '/picture/thumbnail/list', function(data) {
		$.each(data, function(i, thumbnail) {
			 var thumb = $('<li class="thumbnail"></li>');
			 var image = $('<img src="' + thumbnail['url'] + '"/>');
			 $(image).click(function() {
					  self.selectPicture(thumbnail['id']);
					});
			 $(thumb).append(image);
			 $('#design-center .picture-selector ul').append(thumb);
		       });
	      });
  };
  this.init = function() {
    this.loadThumbnails();
  };
}
