import {React} from 'react';
import {Menu} from 'antd';
import {FontAwesomeIcon} from '@fortawesome/react-fontawesome'
import {faCircle} from '@fortawesome/free-solid-svg-icons'

function getItem(label, key, icon, children, type) {
    return {key, icon, children, label, type};
}

const QuickAccess = (props) => {
    var i = 0;
    let favorite = props.favorite.map((x) => {i = i + 1; return getItem(x.title, i - 1)});
    let tags = props.tags.map((x) => {
        i = i + 1;
        return getItem(x.name, i - 1,
            <FontAwesomeIcon
                icon={faCircle}
                style={{color: 'rgba(' + x.red + ',' + x.green + ',' + x.blue + ',' + x.alpha + ')'}} />)
    });
    let recent = props.recent.map((x) => {i = i + 1; return getItem(x, i - 1)});
    let menuItems = [
        getItem('Favorite', 'favorite', null, favorite),
        getItem('Tags', 'tags', null, tags),
        getItem('Recent', 'recent', null, recent),
    ];
    return (
        <Menu
            items={menuItems}
            mode='inline'
            defaultOpenKeys={['favorite', 'recent', 'tags']}
            inlineIndent='15'
            style={{
                height: '100vh',
                overflowY: 'scroll',
                scrollbarWidth: 'none',
                backgroundColor: 'transparent',
            }}
        />
    )
}

export default QuickAccess;
