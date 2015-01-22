
import java.util.WeakHashMap;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Iterator;

import java.awt.Color;

/**
 * Each concrete sub-aspect of Observing defines one kind of observing
 * relationship.  Within that kind of relationship, there can be any number
 * of subjects, each with any number of observers.
 *
 * The sub-aspect defines three things:
 *
 *    (i) what types can be subjects or observers
 *        this is done using +implements
 *
 *   (ii) what operations on the subject require updating the observers
 *        this is done by concretizing the changes(Subject) pointcut
 *
 *  (iii) how to update the observers
 *        this is done by defining a method on
 *        updateObserver(Subject, Observer)
 *
 * Note that in this implementation, the work of updating is a method
 * on the sub-aspect, not a method introduced on the observer.  This
 * allows one class of object to be the observer in different kinds of
 * observing relationships, each of which has a different updating
 * behavior.  For observers that just have a single generic update
 * behavior, the method on updateObserver will just be a simple call
 * that generic updater.
 * 
 */
abstract aspect Observing of eachJVM() {
    
    private WeakHashMap perSubjectObservers = new WeakHashMap();

    private Collection getObservers(Subject s) {
	Collection observers = (Collection)perSubjectObservers.get(s);
	if ( observers == null ) {
	    observers = new LinkedList();
	    perSubjectObservers.put(s, observers);
	}
	return observers;
    }

    
    /**
     * This interface is used by extending aspects to say what types
     * can be subjects.
     */
    protected interface Subject  { }    

    /**
     * This interface is used by extending aspects to say what types
     * can be subjects.
     */
    protected interface Observer { }

    
    /**
     * This is the equivalent of attach.
     */
    public void    addObserver(Subject s, Observer o) { getObservers(s).add(o);    }
    public void removeObserver(Subject s, Observer o) { getObservers(s).remove(o); }


    /**
     * The join points after which to do the update.
     * It replaces the normally scattered <i>calls to</i> notify.
     */
    abstract pointcut changes(Subject s);


    /**
     * Call updateObserver to update each observer.
     */
    after(Subject s): changes(s) {
	Iterator iter = getObservers(s).iterator();
	while ( iter.hasNext() ) {
	    updateObserver(s, ((Observer)iter.next()));
	}
    }

    abstract void updateObserver(Subject s, Observer o);
}
