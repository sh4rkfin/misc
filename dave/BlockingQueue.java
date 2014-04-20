package dave;

import java.util.LinkedList;

/**
*/
public class BlockingQueue<E>
{
    private LinkedList<E> _queue;
    private int _max;

    public BlockingQueue (int max)
    {
        _queue = new LinkedList<E>();
        _max = max;
    }

    public boolean enqueue (E element)
    {
        if (Thread.interrupted()) {
            return false;
        }
        synchronized (_queue) {
            try {
                while (_queue.size() >= _max) {
                    _queue.wait();
                }
                _queue.add(element);
            }
            catch (InterruptedException ex) {
                return false;
            }
            finally {
                _queue.notify();
            }
        }
        return true;
    }

    public E dequeue (long millisToWait)
    {
        if (Thread.interrupted()) {
            return null;
        }
        long start = millisToWait > 0 ? System.currentTimeMillis() : 0;
        synchronized (_queue) {
            try {
                while (_queue.isEmpty()) {
                    long waitTime = 0;
                    if (millisToWait > 0) {
                        long now = System.currentTimeMillis();
                        waitTime = millisToWait - now + start;
                        if (waitTime <= 0) {
                            return null;
                        }
                    }
                    _queue.wait(waitTime);
                }
                return _queue.removeFirst();
            }
            catch (InterruptedException ex) {
                // we were interrupted, quit
                return null;
            }
            finally {
                _queue.notify();
            }
        }
    }
}
