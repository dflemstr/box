$(function(){
    var orderingField = $('#ordering-field'),
    directionField = $('#direction-field'),
    searchField = $('#search-field'),
    def = 'default',
    updateSearch = function() {
        var d = directionField.val();
        searchField.val((searchField.val().replace(/orderby:[a-z]+/, '').trim() +
            ' orderby:' + orderingField.val() + ((d == def) ? '' : d)).trim());
    },
    search = searchField.val(),
    ordersearch = search.match('orderby:(author|pxmlid|rating|time|title)(desc|asc|)');
    if(ordersearch && ordersearch.length == 3) {
        orderingField.val(ordersearch[1]);
        directionField.val((ordersearch[2].length == 0)?def:ordersearch[2]);
    }

    orderingField.change(updateSearch);
    directionField.change(updateSearch);
});