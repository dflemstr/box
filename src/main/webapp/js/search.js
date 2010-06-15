$(function(){
    var orderingField = $('#ordering-field'),
        directionField = $('#direction-field'),
        searchField = $('#search-field'),
        title = 'title',
        rating = 'rating',
        time = 'time',
        def = 'default',
        asc = 'asc',
        desc = 'desc',
        updateSearch = function() {
            var d = directionField.val();
            searchField.val((searchField.val().replace(/orderby:[a-z]+/, '').trim() +
                ' orderby:' + orderingField.val() + ((d == def) ? '' : d)).trim());
        },
        search = searchField.val(),
        ordering = search.match('orderby:([a-z]+)');
    if(ordering) {
        ordering = ordering[1];
        if(ordering == title) {
            orderingField.val(title);
            directionField.val(def);
        } else if(ordering == title + asc) {
            orderingField.val(title);
            directionField.val(asc);
        } else if(ordering == title + desc) {
            orderingField.val(title);
            directionField.val(desc);
        } else if(ordering == rating) {
            orderingField.val(rating);
            directionField.val(def);
        } else if(ordering == rating + asc) {
            orderingField.val(rating);
            directionField.val(asc);
        } else if(ordering == rating + desc) {
            orderingField.val(rating);
            directionField.val(desc);
        } else if(ordering == time) {
            orderingField.val(time);
            directionField.val(def);
        } else if(ordering == time + asc) {
            orderingField.val(time);
            directionField.val(asc);
        } else if(ordering == time + desc) {
            orderingField.val(time);
            directionField.val(desc);
        }
    }

    orderingField.change(updateSearch);
    directionField.change(updateSearch);
});