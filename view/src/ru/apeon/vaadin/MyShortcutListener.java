package ru.apeon.vaadin;

import com.vaadin.event.ShortcutListener;

/**
 * @author Anton Zherdev
 */
public abstract class MyShortcutListener extends ShortcutListener {
    public MyShortcutListener(String caption, int keyCode, int... modifierKeys) {
        super(caption, keyCode, modifierKeys);
    }
}
