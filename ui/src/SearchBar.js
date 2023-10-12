import {React, useState} from 'react';
import {SearchOutlined} from '@ant-design/icons';
import {Tag, Input, Select} from 'antd';

const OPTIONS = ['Apples', 'Nails', 'Bananas', 'Helicopters'];

const tagRender = (props) => {
    const {label, value, closable, onClose} = props;
    const onPreventMouseDown = (event) => {
        event.preventDefault();
        event.stopPropagation();
    };
    return (
        <Tag
            onMouseDown={onPreventMouseDown}
            closable={closable}
            onClose={onClose}
            style={{marginRight: 3, fontWeight: 'bold'}}
        >
            {label}
        </Tag>
    );
};

const SearchBar = () => {
    const [selectedItems, setSelectedItems] = useState([]);
    const filteredOptions = OPTIONS.filter((o) => !selectedItems.includes(o));
    return (
        <div style={{display: 'inline', whiteSpace: 'nowrap', overflow: 'scroll'}}>
            <Select
                showSearch
                allowClear
                tagRender={tagRender}
                mode="multiple"
                suffixIcon={null}
                autoFocus={true}
                placeholder="Filter Tags"
                maxTagCount={'responsive'}
                style={{width: '25em', marginRight: '1em', textAlign: 'left'}}
                value={selectedItems}
                onChange={setSelectedItems}
                options={filteredOptions.map((item) => ({value: item, label: item}))}
            />
            <Input
                allowClear
                placeholder="Search"
                style={{display: 'inline-flex', width: '15em'}}
                prefix={<SearchOutlined className="site-form-item-icon" />}
            />
        </div>);
};

export default SearchBar;
